with Ada.Text_IO; use Ada.Text_IO;
with Timing_Controller; use Timing_Controller;

procedure Test_Timer is
begin
   -- Initialize the headlight timer
   -- you only call entries on a task instance and not the type
   Put_Line("Test: Initializing headlight timer.");
   Put_Line("Test: Setting headlight timer for 5 seconds.");
   Timing_Controller.Headlight_Timer_task.Set_Headlight_Timer(5.0);

   -- Wait for a duration longer than the timer to observe the output
   delay 6.0;

   Put_Line("Test: Shutting down headlight timer.");
   Timing_Controller.Headlight_Timer_task.Shutdown;
   

   -- Allow time for the shutdown message to be displayed
   delay 1.0;
end Test_Timer;
