
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

package body Timing_Controller is
   -- This package implements the timing controller for the vehicle system.
   -- It includes procedures to manage the timing of vehicle operations.
   -- The package uses Ada.Real_Time for time management.
   -- This procedure sets the timer for a specified duration in seconds.
   -- It takes a duration as input and waits for that duration before proceeding.
   task body Headlight_Timer is
      Delay_Timed : Duration := 1.0;  -- Duration in seconds
      Requested : Boolean := False;
      Termin : Boolean := False;
   begin
      loop
         select
            accept Set_Headlight_Timer(Delay_Time : Duration) do
               Requested := True;
               Delay_Timed := Delay_Time;
            end Set_Headlight_Timer;
         or
            accept Shutdown do
               Termin := True;
               Put_Line("Headlight timer is shutting down.");
            end Shutdown;
         end select;
         if Termin then
            exit;
         end if;
            if Requested then
               Put_Line("Headlight will turn on in " & Duration'Image(Delay_Timed) & " seconds.");
               delay Delay_Timed;
               Requested := False;
            end if;
      end loop;
   end Headlight_Timer;
      

end Timing_Controller;