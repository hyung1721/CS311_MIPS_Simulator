#define True 1

typedef int semaphore;

/* global variable that is shared among all threads */
int nBottles = 0; //number of wine bottles in the refrigerator.
semaphore sema_nBottles;

void customer_run() {
    do {
        sema_down (&sema_nBottles);
        if (nBottles != 0) {
            /* The customer drinks a random number(i.e., [1, nBottles]) of bottles of 
            6 wine from the refrigerator */
            int amount = random(1, nBottles);
     	    nBottles = nBottles - amount;
        }
        sema_up (&sema_nBottles);
   } while(True);
}

void drone_run() { //
    do {
        /* When refrigerator is not full, WineDrone tries to make it full */
        sema_down (&sema_nBottles);
         if (nBottles < 10) {
            int buy_amount = 10 - nBottles;         	
            /*drone deilvers the bottles of wine*/
            nBottles = nBottles + buy_amount;
         }
         sema_up (&sema_nBottles);
     } while(True);
 }
