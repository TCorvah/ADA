with Ada.Real_Time; use Ada.Real_Time;

package Timing_Controller is
   task type Headlight_Timer is
      entry Set_Headlight_Timer(Delay_Time : Duration);
      entry Shutdown;
   end Headlight_Timer;

   -- This task implements the headlight timer functionality.
   -- It waits for a specified duration before proceeding with the headlight operation.
   -- The task uses Ada.Real_Time for time management.
   Headlight_Timer_task : Headlight_Timer;

   task type Garage_Movement_Controller is
      entry Start_Garage_Movement_Timer(Start_Movements : Float; Maximum_Speed : Float);
      entry Shutdown;
   end Garage_Movement_Controller;

   Garage_Movement_Controller_task : Garage_Movement_Controller;
   
end Timing_Controller;
