/***************************************************************/
/*                                                             */
/*   MIPS-32 Instruction Level Simulator                       */
/*                                                             */
/*   CS311 KAIST                                               */
/*   run.c                                                     */
/*                                                             */
/***************************************************************/

#include <stdio.h>

#include "util.h"
#include "run.h"

/***************************************************************/
/*                                                             */
/* Procedure: get_inst_info                                    */
/*                                                             */
/* Purpose: Read insturction information                       */
/*                                                             */
/***************************************************************/
instruction* get_inst_info(uint32_t pc) { 
    return &INST_INFO[(pc - MEM_TEXT_START) >> 2];
}


/***************************************************************/
/*                                                             */
/* Procedure: process_instruction                              */
/*                                                             */
/* Purpose: Process one instrction                             */
/*                                                             */
/***************************************************************/
void process_instruction(){
    CPU_State CPU_STATE_TEMP = CURRENT_STATE; // copying CURRENT_STATE

    // IF Stage
    if(!(CPU_STATE_TEMP.PIPE_STALL[IF_STAGE])){
        if(!CPU_STATE_TEMP.STALL){
	        instruction *inst;

            CURRENT_STATE.IF_PC = (CPU_STATE_TEMP.PC == 0x400000)? 0x400000 : CPU_STATE_TEMP.PC;
            
		    if(CPU_STATE_TEMP.PC_SRC){
		        CURRENT_STATE.PC = CPU_STATE_TEMP.BRANCH_PC;
			    CURRENT_STATE.PC_SRC = 0;
			    if(CPU_STATE_TEMP.JUMP){
	                CURRENT_STATE.JUMP = 0;
			    }
		    }
		    else{
			    if(CPU_STATE_TEMP.JUMP){
				    CURRENT_STATE.PC = CPU_STATE_TEMP.JUMP_PC;
				    CURRENT_STATE.JUMP = 0;
			    }
			    else{
			        CURRENT_STATE.PC = CURRENT_STATE.IF_PC;
			    }
		    } 

		    inst = get_inst_info(CURRENT_STATE.PC);

		    // last instruction PC + 4 detected
		    if(CURRENT_STATE.PC == (0x400000 + NUM_INST * 4)){
		        CURRENT_STATE.IF_ID_PC = 0;
			    CURRENT_STATE.PIPE_STALL[IF_STAGE] = TRUE;
			    CURRENT_STATE.PIPE_STALL[ID_STAGE] = TRUE;
			    CURRENT_STATE.PIPE[IF_STAGE] = 0;
		    }
		    else{
		        CURRENT_STATE.IF_ID_PC = CURRENT_STATE.PC;
			    CURRENT_STATE.IF_ID_REG1 = RS(inst);
			    CURRENT_STATE.IF_ID_REG2 = RT(inst);
			    CURRENT_STATE.PIPE[IF_STAGE] = CURRENT_STATE.IF_ID_PC;
			    CURRENT_STATE.PC += 4;
		    }
		}
	}
    else{ // if IF stage is stalled
        CURRENT_STATE.IF_ID_INST = 0;
        CURRENT_STATE.IF_ID_REG1 = 0;
        CURRENT_STATE.IF_ID_REG2 = 0;
        CURRENT_STATE.PIPE[IF_STAGE] = 0;

		CURRENT_STATE.ID_TEMP_STALL = TRUE;
		CURRENT_STATE.PIPE_STALL[ID_STAGE] = TRUE;

		if(CPU_STATE_TEMP.PC == (0x400000 + NUM_INST*4)){
		    CURRENT_STATE.PIPE[ID_STAGE] = 0;
			CURRENT_STATE.PIPE_STALL[IF_STAGE] = TRUE;
		}
		else CURRENT_STATE.PIPE_STALL[IF_STAGE] = FALSE;    
    }

    // WB stage: first half of clock cycle
    if(CPU_STATE_TEMP.MEM_WB_PC != 0 && !(CPU_STATE_TEMP.PIPE_STALL[WB_STAGE])){
        unsigned char dest = CPU_STATE_TEMP.MEM_WB_DEST;
		if((dest >=0) && (dest <=31) && CPU_STATE_TEMP.MEM_WB_REG_WRITE){
			if(CPU_STATE_TEMP.MEM_WB_MEM_TO_REG){
                CURRENT_STATE.REGS[dest] = CPU_STATE_TEMP.MEM_WB_MEM_OUT;
			}
			else CURRENT_STATE.REGS[dest] = CPU_STATE_TEMP.MEM_WB_ALU_OUT;
		}
        
        CURRENT_STATE.PIPE[WB_STAGE] = CPU_STATE_TEMP.MEM_WB_PC;
		
		if(CPU_STATE_TEMP.MEM_WB_PC != 0) INSTRUCTION_COUNT++;
		
        if(CPU_STATE_TEMP.MEM_WB_PC == (0x400000 + (NUM_INST-1) * 4)){
		    RUN_BIT = FALSE;
		}
	}
    else CURRENT_STATE.PIPE[WB_STAGE] = 0;   

    // ID stage
    if(CPU_STATE_TEMP.IF_ID_PC != 0 && !(CPU_STATE_TEMP.PIPE_STALL[ID_STAGE])){
        if(!CPU_STATE_TEMP.STALL){
		    instruction *inst = get_inst_info(CPU_STATE_TEMP.IF_ID_PC);
	  	
            CURRENT_STATE.ID_EX_PC = CPU_STATE_TEMP.IF_ID_PC;
		    CURRENT_STATE.PIPE[ID_STAGE] = CPU_STATE_TEMP.IF_ID_PC;

            CURRENT_STATE.ID_EX_OPCODE = OPCODE(inst);
            CURRENT_STATE.ID_EX_FUNCT  = FUNC(inst);
		    CURRENT_STATE.ID_EX_REG1   = RS(inst);
            CURRENT_STATE.ID_EX_REG2   = RT(inst);
            CURRENT_STATE.ID_EX_DEST   = RD(inst);
            CURRENT_STATE.ID_EX_SHAMT  = SHAMT(inst);
            CURRENT_STATE.ID_EX_IMM    = IMM(inst);
            CURRENT_STATE.ID_EX_TARGET = TARGET(inst);
		
		    CURRENT_STATE.ID_EX_REG1V = CPU_STATE_TEMP.REGS[RS(inst)];
		    CURRENT_STATE.ID_EX_REG2V = CPU_STATE_TEMP.REGS[RT(inst)];

            if(CPU_STATE_TEMP.MEM_WB_REG_WRITE){
			     if(CPU_STATE_TEMP.MEM_WB_DEST != 0 && CPU_STATE_TEMP.MEM_WB_DEST == CURRENT_STATE.ID_EX_REG1)
                    CURRENT_STATE.ID_EX_REG1V = CURRENT_STATE.REGS[RS(inst)];
            }
        
            if(CPU_STATE_TEMP.MEM_WB_REG_WRITE){
                if(CPU_STATE_TEMP.MEM_WB_DEST != 0 && CPU_STATE_TEMP.MEM_WB_DEST == CURRENT_STATE.ID_EX_REG2)
                    CURRENT_STATE.ID_EX_REG2V = CURRENT_STATE.REGS[RT(inst)];
            }
        
            int op = OPCODE(inst);
           
            if(op == 0x2b){
                if(CPU_STATE_TEMP.MEM_WB_REG_WRITE && CPU_STATE_TEMP.MEM_WB_DEST != 0 && CPU_STATE_TEMP.MEM_WB_DEST == CURRENT_STATE.ID_EX_DEST)
                    CURRENT_STATE.ID_EX_DESTV = CURRENT_STATE.REGS[RD(inst)];
            }
            
            // control signal
            CURRENT_STATE.ID_EX_REG_DST    = (op == 0x0) ? 1 : 0;
            CURRENT_STATE.ID_EX_ALU_SRC    = (op == 0x9 || op == 0xc  || op == 0xf  || op == 0xd 
                                           || op == 0xb || op == 0x23 || op == 0x2b || op == 0x4 
                                           || op == 0x5 ) ? 1 : 0;
            CURRENT_STATE.ID_EX_BRCH       = (op == 0x4  || op == 0x5) ? 1 : 0;
            CURRENT_STATE.ID_EX_MEM_READ   = (op == 0x23) ? 1 : 0;
            CURRENT_STATE.ID_EX_MEM_WRITE  = (op == 0x2b) ? 1 : 0;
            CURRENT_STATE.ID_EX_REG_WRITE  = (op == 0x9 || op == 0xc  || op == 0xf  || op == 0xd
									       || op == 0xb || op == 0x23 || op == 0x0) ? 1 : 0;
            CURRENT_STATE.ID_EX_MEM_TO_REG = (op == 0x23) ? 1 : 0;
		
		    if(!CURRENT_STATE.ID_EX_REG_DST) CURRENT_STATE.ID_EX_DEST = CURRENT_STATE.ID_EX_REG2;

		    if(CURRENT_STATE.ID_EX_BRCH) CURRENT_STATE.ID_EX_ALU_SRC = 0;

            // jump instructions cause flushes
            if(op == 0x2 || op == 0x3 || (op == 0x0 && FUNC(inst) == 0x08)){
			    CURRENT_STATE.JUMP_PC = TARGET(inst) << 2;
                CURRENT_STATE.JUMP = TRUE;

                if(op == 0x3) CURRENT_STATE.REGS[31] = CPU_STATE_TEMP.IF_ID_PC + 4;
                if(op == 0x0 && FUNC(inst) == 0x08){
                    CURRENT_STATE.JUMP_PC = CPU_STATE_TEMP.REGS[RS(inst)];
                }
			
			    CURRENT_STATE.PIPE_STALL[ID_STAGE] = TRUE;
		    }
		}
		else{
            if(CPU_STATE_TEMP.IF_ID_PC != 0){
		        instruction *inst = get_inst_info(CPU_STATE_TEMP.ID_EX_PC);
				if(CPU_STATE_TEMP.MEM_WB_REG_WRITE){
				    if(CPU_STATE_TEMP.MEM_WB_DEST != 0 && CPU_STATE_TEMP.MEM_WB_DEST == CPU_STATE_TEMP.ID_EX_REG1)
				        CURRENT_STATE.ID_EX_REG1V = CURRENT_STATE.REGS[RS(inst)];
			    }
		   	    if(CPU_STATE_TEMP.MEM_WB_REG_WRITE){
			        if(CPU_STATE_TEMP.MEM_WB_DEST != 0 && CPU_STATE_TEMP.MEM_WB_DEST == CPU_STATE_TEMP.ID_EX_REG2)
				        CURRENT_STATE.ID_EX_REG2V = CURRENT_STATE.REGS[RT(inst)];
			    }
			}
			CURRENT_STATE.STALL = FALSE;
		}
    }
	else{ 
        CURRENT_STATE.ID_EX_PC    = 0;
        CURRENT_STATE.ID_EX_DEST  = 0;
        CURRENT_STATE.ID_EX_REG1  = 0;
        CURRENT_STATE.ID_EX_REG2  = 0;
        CURRENT_STATE.ID_EX_REG1V  = 0;
        CURRENT_STATE.ID_EX_REG2V  = 0;
        CURRENT_STATE.ID_EX_IMM   = 0;
        CURRENT_STATE.ID_EX_SHAMT = 0;
        CURRENT_STATE.ID_EX_TARGET= 0;
		CURRENT_STATE.PIPE[ID_STAGE] = 0;

	    CURRENT_STATE.EX_TEMP_STALL = TRUE;
		CURRENT_STATE.PIPE_STALL[EX_STAGE] = TRUE;

        CURRENT_STATE.ID_EX_REG_DST    = 0;
        CURRENT_STATE.ID_EX_ALU_SRC    = 0;
        CURRENT_STATE.ID_EX_BRCH       = 0;
        CURRENT_STATE.ID_EX_MEM_READ   = 0;
        CURRENT_STATE.ID_EX_MEM_WRITE  = 0;
        CURRENT_STATE.ID_EX_REG_WRITE  = 0;
        CURRENT_STATE.ID_EX_MEM_TO_REG = 0;

    }

    // EX stage
    if(CPU_STATE_TEMP.ID_EX_PC !=0 && !(CPU_STATE_TEMP.PIPE_STALL[EX_STAGE])){
		int rs     = CPU_STATE_TEMP.ID_EX_REG1;
        int rt     = CPU_STATE_TEMP.ID_EX_REG2;
        int rd     = CPU_STATE_TEMP.ID_EX_DEST;
        int imm    = CPU_STATE_TEMP.ID_EX_IMM;
        int shamt  = CPU_STATE_TEMP.ID_EX_SHAMT;
        int target = CPU_STATE_TEMP.ID_EX_TARGET;

        int rs_value = CPU_STATE_TEMP.ID_EX_REG1V;
        int rt_value = CPU_STATE_TEMP.ID_EX_REG2V;

		CURRENT_STATE.EX_MEM_DEST = CPU_STATE_TEMP.ID_EX_DEST;

        if (CPU_STATE_TEMP.EX_MEM_REG_WRITE && (CPU_STATE_TEMP.EX_MEM_DEST != 0)
		    && (CPU_STATE_TEMP.EX_MEM_DEST == CPU_STATE_TEMP.ID_EX_REG1)) CURRENT_STATE.EX_FORWARD_REG1 = 1;
        if (CPU_STATE_TEMP.EX_MEM_REG_WRITE && (CPU_STATE_TEMP.EX_MEM_DEST != 0)
            && (CPU_STATE_TEMP.EX_MEM_DEST == CPU_STATE_TEMP.ID_EX_REG2)) CURRENT_STATE.EX_FORWARD_REG2 = 1;
        
        if (CPU_STATE_TEMP.MEM_WB_REG_WRITE && (CPU_STATE_TEMP.MEM_WB_DEST != 0)
            && (CPU_STATE_TEMP.EX_MEM_DEST != CPU_STATE_TEMP.ID_EX_REG1)
            && (CPU_STATE_TEMP.MEM_WB_DEST == CPU_STATE_TEMP.ID_EX_REG1)) CURRENT_STATE.EX_FORWARD_REG1 = 2;
        if (CPU_STATE_TEMP.MEM_WB_REG_WRITE && (CPU_STATE_TEMP.MEM_WB_DEST != 0)
            && (CPU_STATE_TEMP.EX_MEM_DEST != CPU_STATE_TEMP.ID_EX_REG2)
            && (CPU_STATE_TEMP.MEM_WB_DEST == CPU_STATE_TEMP.ID_EX_REG2)) CURRENT_STATE.EX_FORWARD_REG2 = 2;

        int REGS_RS;
        int REGS_RT;

        int result_ALU  = 0;
        int result_WORD = 0;

        switch(CURRENT_STATE.EX_FORWARD_REG1){
            case 0: // original data path
                REGS_RS = rs_value;
                break;
            case 1: // EX/MEM to EX forwarding
                REGS_RS = CPU_STATE_TEMP.EX_MEM_ALU_OUT;
                break;
            case 2: // MEM/WB to EX forwarding
                if(CURRENT_STATE.MEM_WB_MEM_TO_REG) REGS_RS = CPU_STATE_TEMP.MEM_WB_MEM_OUT;
                else REGS_RS = CPU_STATE_TEMP.MEM_WB_ALU_OUT;
                break;
        }

        switch(CURRENT_STATE.EX_FORWARD_REG2){
            case 0:
                REGS_RT = rt_value;
                break;
            case 1:
                REGS_RT = CPU_STATE_TEMP.EX_MEM_ALU_OUT;
                break;
            case 2:
                if(CURRENT_STATE.MEM_WB_MEM_TO_REG) REGS_RT = CPU_STATE_TEMP.MEM_WB_MEM_OUT;
                else REGS_RT = CPU_STATE_TEMP.MEM_WB_ALU_OUT;
                break;
        }

        result_WORD = REGS_RT;
        if(CPU_STATE_TEMP.ID_EX_ALU_SRC) REGS_RT = imm;
        if(CPU_STATE_TEMP.ID_EX_OPCODE == 0x0 && (CPU_STATE_TEMP.ID_EX_FUNCT == 0x02 || CPU_STATE_TEMP.ID_EX_FUNCT == 0x00))
		    REGS_RS = shamt;

        //forwarding bit initialize
        CURRENT_STATE.EX_FORWARD_REG1 = 0;
        CURRENT_STATE.EX_FORWARD_REG2 = 0;

        switch (CPU_STATE_TEMP.ID_EX_OPCODE){
            case 0x9:  // addiu
                result_ALU = REGS_RS + REGS_RT;
                break;
            case 0xc:  // andi
                result_ALU = REGS_RS & REGS_RT;
                break;
            case 0xf:  // lui
                result_ALU = REGS_RT << 16;
                break;
            case 0xd:  // ori
                result_ALU = REGS_RS | REGS_RT;
                break;
            case 0xb:  // sltiu
                result_ALU = (REGS_RS < SIGN_EX(REGS_RT)) ? 1 : 0;
                break;
            case 0x23: // lw
                result_ALU = REGS_RS + REGS_RT;
                break;
            case 0x2b: // sw
                result_ALU = REGS_RS + REGS_RT;
                break;
            case 0x4:  // beq
                if(REGS_RS == REGS_RT){
                    CURRENT_STATE.EX_MEM_BR_TAKE   = TRUE; 
                    CURRENT_STATE.EX_MEM_BR_TARGET = CPU_STATE_TEMP.PC + REGS_RT * 4;
                }
                break;
            case 0x5:  // bne
                if(REGS_RS != REGS_RT){
                    CURRENT_STATE.EX_MEM_BR_TAKE   = TRUE; 
                    CURRENT_STATE.EX_MEM_BR_TARGET = CPU_STATE_TEMP.PC + REGS_RT * 4;
                }
                break;
            case 0x0:  // R type
                switch (CPU_STATE_TEMP.ID_EX_FUNCT) {
                    case 0x21:  // addu
                        result_ALU = REGS_RS + REGS_RT;
                        break;
                    case 0x24:  // and
                        result_ALU = REGS_RS & REGS_RT;
                        break;
                    case 0x27:  // nor
                        result_ALU = ~ ( REGS_RS | REGS_RT );
                        break;
                    case 0x25:  // or
                        result_ALU = REGS_RS | REGS_RT;
                        break;
                    case 0x2b:  // sltu
                        result_ALU = (REGS_RS < REGS_RT) ? 1 : 0;
                        break;
                    case 0x0:  // sll
                        result_ALU = REGS_RT << shamt;
                        break;
                    case 0x02: // srl
                        result_ALU = ((REGS_RT >> shamt) & 0x7fffffff);
                        break;
                    case 0x23: // subu
                        result_ALU = REGS_RS - REGS_RT;
                        break;
                    case 0x08: // jr
                        CURRENT_STATE.EX_MEM_JUMP = REGS_RS;
                        break;
                    default:
                        assert(0);
                }
                break;
            case 0x2:  // j
                break;
            case 0x3:  // jal
                break;
            default:
                assert(0);
        }
        
		// load-use hazard detection unit
        if(CPU_STATE_TEMP.ID_EX_MEM_READ){
		    if(CPU_STATE_TEMP.ID_EX_REG2 == CPU_STATE_TEMP.IF_ID_REG1
			|| CPU_STATE_TEMP.ID_EX_REG2 == CPU_STATE_TEMP.IF_ID_REG2)
			   CURRENT_STATE.STALL = TRUE;
		}

		if(CURRENT_STATE.STALL){
		    CURRENT_STATE.PIPE_STALL[EX_STAGE] = TRUE;
		}

		CURRENT_STATE.EX_MEM_PC = CPU_STATE_TEMP.ID_EX_PC;
        
        if(CURRENT_STATE.EX_MEM_BR_TAKE){
			CURRENT_STATE.PC_SRC = TRUE;
			CURRENT_STATE.BRANCH_PC = CURRENT_STATE.EX_MEM_PC + 4 + (imm << 2);
			CURRENT_STATE.PIPE_STALL[IF_STAGE] = TRUE;
			CURRENT_STATE.PIPE_STALL[ID_STAGE] = TRUE;
			CURRENT_STATE.PIPE_STALL[EX_STAGE] = TRUE;
			if(CURRENT_STATE.JUMP) CURRENT_STATE.JUMP = FALSE;
		}

        CURRENT_STATE.EX_MEM_OPCODE    = CPU_STATE_TEMP.ID_EX_OPCODE;
        CURRENT_STATE.EX_MEM_ALU_OUT   = result_ALU;
        CURRENT_STATE.EX_MEM_W_VALUE   = result_WORD;
        CURRENT_STATE.EX_MEM_REG2      = rt;
        CURRENT_STATE.EX_MEM_IMM       = imm;
        CURRENT_STATE.EX_MEM_DESTV     = CPU_STATE_TEMP.ID_EX_DESTV;
        CURRENT_STATE.EX_MEM_FUNCT     = CPU_STATE_TEMP.ID_EX_FUNCT;
        CURRENT_STATE.EX_MEM_REG_WRITE = CPU_STATE_TEMP.ID_EX_REG_WRITE;

        CURRENT_STATE.PIPE[EX_STAGE] = CPU_STATE_TEMP.ID_EX_PC;

        CURRENT_STATE.EX_MEM_BRCH       = CPU_STATE_TEMP.ID_EX_BRCH;
        CURRENT_STATE.EX_MEM_MEM_READ   = CPU_STATE_TEMP.ID_EX_MEM_READ;
        CURRENT_STATE.EX_MEM_MEM_WRITE  = CPU_STATE_TEMP.ID_EX_MEM_WRITE;
        CURRENT_STATE.EX_MEM_REG_WRITE  = CPU_STATE_TEMP.ID_EX_REG_WRITE;
        CURRENT_STATE.EX_MEM_MEM_TO_REG = CPU_STATE_TEMP.ID_EX_MEM_TO_REG;

    }
    else{
		CURRENT_STATE.EX_MEM_ALU_OUT   = 0;
        CURRENT_STATE.EX_MEM_BR_TAKE   = 0;
        CURRENT_STATE.EX_MEM_BR_TARGET = 0;
        CURRENT_STATE.EX_MEM_DEST      = 0;
        CURRENT_STATE.EX_MEM_JUMP      = 0;
        CURRENT_STATE.EX_MEM_OPCODE    = 0;
        CURRENT_STATE.EX_MEM_PC        = 0;
        CURRENT_STATE.EX_MEM_W_VALUE   = 0;
        CURRENT_STATE.EX_MEM_FUNCT     = 0;
        CURRENT_STATE.EX_MEM_REG_WRITE = FALSE;
        CURRENT_STATE.EX_MEM_DESTV     = 0;

		CURRENT_STATE.PIPE[EX_STAGE] = 0;
		CURRENT_STATE.MEM_TEMP_STALL = TRUE;
        CURRENT_STATE.PIPE_STALL[MEM_STAGE] = TRUE;

        CURRENT_STATE.EX_MEM_BRCH       = 0;
        CURRENT_STATE.EX_MEM_MEM_READ   = 0;
        CURRENT_STATE.EX_MEM_MEM_WRITE  = 0;
        CURRENT_STATE.EX_MEM_REG_WRITE  = 0;
        CURRENT_STATE.EX_MEM_MEM_TO_REG = 0;
    }

    // MEM stage
    if(CPU_STATE_TEMP.EX_MEM_PC != 0 && !(CPU_STATE_TEMP.PIPE_STALL[MEM_STAGE])){
		uint32_t word;
        uint32_t dest_value = CPU_STATE_TEMP.EX_MEM_DESTV;
        short op = CPU_STATE_TEMP.EX_MEM_OPCODE;

        // Mem to mem copy forwarding unit
        if(CPU_STATE_TEMP.MEM_WB_MEM_TO_REG && CPU_STATE_TEMP.EX_MEM_MEM_WRITE
        && CPU_STATE_TEMP.MEM_WB_DEST !=0 && CPU_STATE_TEMP.MEM_WB_DEST == CPU_STATE_TEMP.EX_MEM_REG2)
            CURRENT_STATE.MEM_WB_FORWARD_COPY = 1;

        if(CURRENT_STATE.MEM_WB_FORWARD_COPY) word = CPU_STATE_TEMP.MEM_WB_MEM_OUT;
        else word = CPU_STATE_TEMP.EX_MEM_ALU_OUT;
        
        if(CURRENT_STATE.MEM_WB_FORWARD_COPY){
            mem_write_32(CPU_STATE_TEMP.MEM_WB_MEM_OUT, dest_value);
        }
        else{
            switch(op){
                case 0x23:
                    CURRENT_STATE.MEM_WB_MEM_OUT = (word != 0) ? mem_read_32(word) : 0;
                    break;
                case 0x2b:
                    if(word != 0) mem_write_32(word, dest_value);
                    break;
                case 0x9:
                case 0xc:
                case 0xf:
                case 0xd:
                case 0xb:
                case 0x0:
                    CURRENT_STATE.MEM_WB_ALU_OUT = CPU_STATE_TEMP.EX_MEM_ALU_OUT;
                    break;
            }
        }
        CURRENT_STATE.MEM_WB_DEST     = CPU_STATE_TEMP.EX_MEM_DEST;
        CURRENT_STATE.MEM_WB_PC       = CPU_STATE_TEMP.EX_MEM_PC;
        CURRENT_STATE.PIPE[MEM_STAGE] = CPU_STATE_TEMP.EX_MEM_PC;

        CURRENT_STATE.MEM_WB_REG_WRITE  = CPU_STATE_TEMP.EX_MEM_REG_WRITE;
        CURRENT_STATE.MEM_WB_MEM_TO_REG = CPU_STATE_TEMP.EX_MEM_MEM_TO_REG;
	}
    else{
		CURRENT_STATE.MEM_WB_ALU_OUT = 0;
        CURRENT_STATE.MEM_WB_BR_TAKE = 0;
        CURRENT_STATE.MEM_WB_DEST    = 0;
        CURRENT_STATE.MEM_WB_MEM_OUT = 0;
        CURRENT_STATE.MEM_WB_PC      = 0;

		CURRENT_STATE.PIPE[MEM_STAGE] = 0;
        
		CURRENT_STATE.WB_TEMP_STALL = TRUE;
		CURRENT_STATE.PIPE_STALL[WB_STAGE]  = TRUE;

        CURRENT_STATE.MEM_WB_REG_WRITE  = 0;
        CURRENT_STATE.MEM_WB_MEM_TO_REG = 0;
    }

	if(!CURRENT_STATE.EX_MEM_BR_TAKE){
	    if(!CURRENT_STATE.JUMP){
	        if(CURRENT_STATE.ID_TEMP_STALL){
	            CURRENT_STATE.PIPE_STALL[ID_STAGE] = TRUE;
		        CURRENT_STATE.ID_TEMP_STALL = FALSE;
	        }
            else CURRENT_STATE.PIPE_STALL[ID_STAGE] = FALSE;
		}
		if(!CURRENT_STATE.STALL){
	        if(CURRENT_STATE.EX_TEMP_STALL){
	            CURRENT_STATE.PIPE_STALL[EX_STAGE] = TRUE;
		        CURRENT_STATE.EX_TEMP_STALL = FALSE;
	        }
	        else CURRENT_STATE.PIPE_STALL[EX_STAGE] = FALSE;
		}
	}
	else CURRENT_STATE.EX_MEM_BR_TAKE = FALSE;

	if(CURRENT_STATE.MEM_TEMP_STALL){
	    CURRENT_STATE.PIPE_STALL[MEM_STAGE] = TRUE;
	    CURRENT_STATE.MEM_TEMP_STALL = FALSE;
	}
	else CURRENT_STATE.PIPE_STALL[MEM_STAGE] = FALSE;

	if(CURRENT_STATE.WB_TEMP_STALL){
	    CURRENT_STATE.PIPE_STALL[WB_STAGE] = TRUE;
	    CURRENT_STATE.WB_TEMP_STALL = FALSE;
	}
	else CURRENT_STATE.PIPE_STALL[WB_STAGE] = FALSE;
}
