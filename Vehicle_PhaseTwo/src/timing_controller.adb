
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body Timing_Controller is
      package Float_IO is new Ada.Text_IO.Float_IO(Float);
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
            Put_Line("Headlight timer has been terminated.");
            exit;
         end if;
            if Requested then
               Put_Line("Headlight will turn on in " & Duration'Image(Delay_Timed) & " seconds.");
               delay Delay_Timed;
               Requested := False;
            end if;
      end loop;
   end Headlight_Timer;

   task body Garage_Movement_Controller is
      Start_Movement : Float := 0.0;  -- Initial speed
      Max_Speed : Float := 0.0;      -- Maximum speed
      Start : Boolean := False;
      Termin : Boolean := False;
   begin
      loop
         select
            accept Start_Garage_Movement_Timer(Start_Movements : Float; Maximum_Speed : Float) do
               Start := True;
               Start_Movement := Start_Movements;
               Max_Speed := Maximum_Speed;
               Put("Starting garage movement timer with speed: ");
               Float_IO.Put(Item => Start_Movement, Fore => 1, Aft => 2, Exp => 0);
               Put_Line (" m/s ");
               New_Line;
               Put("Maximum speed set to: ");
               Float_IO.Put(Item => Max_Speed, Fore => 1, Aft => 2, Exp => 0);
               Put_Line(" m/s");
               New_Line;
            end Start_Garage_Movement_Timer;
         or
            accept Shutdown do
               Termin := True;
               Put_Line("Garage movement controller is shutting down.");
            end Shutdown;
         end select;
         exit when Termin;
         
         -- If the start flag is set, simulate garage movement
         if Start then
            Float_IO.Put(Item => Start_Movement, Fore => 1, Aft => 2, Exp => 0);
            Put("vehicle preparing to move: ");
            New_Line;
            delay 1.0;  -- Simulate time passing for the garage movement
            while Start_Movement < Max_Speed loop
               Start_Movement := Start_Movement + 5.0;  -- Increment speed
               Put("Vehicle speed increased to: ");
               Float_IO.Put(Item => Start_Movement, Fore => 1, Aft => 2, Exp => 0);
               Put_Line(" m/s");
               delay 1.0;  -- Simulate time passing for each speed increment
            end loop;
            Put("Vehicle Cruising at maximum speed: ");
            Float_IO.Put(Item => Max_Speed, Fore => 1, Aft => 2, Exp => 0);
            Put_Line(" m/s");
            delay 2.0;  -- Simulate time at maximum speed
            while Start_Movement > 0.0 loop
               Start_Movement := Start_Movement - 5.0;  -- Decrement speed
               if Start_Movement < 0.0 then
                  Start_Movement := 0.0;  -- Ensure speed does not go below zero
               end if;

               Put("Vehicle speed decreased to: ");
               Float_IO.Put(Item => Start_Movement, Fore => 1, Aft => 2, Exp => 0);
               Put_Line(" m/s");
               delay 1.0;  -- Simulate time passing for each speed decrement
            end loop;
            Put_Line ("Vehicle has reached destination.");
            Put_Line("Garage movement completed.");
            Start := False;  -- Reset start flag after one cycle
         end if;
      end loop;
   end Garage_Movement_Controller;

      

end Timing_Controller;