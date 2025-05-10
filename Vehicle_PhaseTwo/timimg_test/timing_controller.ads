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
end Timing_Controller;
