with Ada.Text_IO; use Ada.Text_IO;
with Radar_Systems; use Radar_Systems;
with Vehicle_Constants; use Vehicle_Constants;




procedure Test_Radar is
   package Float_IO is new Ada.Text_IO.Float_IO(Float);
   R : Radar;
begin
   Put_Line("=== Radar System Test ===");

   Activate_Radar(R);

   -- Test cases: (Distance, Angle)
   declare
      type Test_Case is record
         Distance : Float;
         Angle    : Float;
      end record;
   
      Test_Cases : constant array (1 .. 6) of Test_Case :=
        (
         (10.0,   2.0),    -- Strong, Close, Front
         (30.0,  20.0),    -- Medium, Close, Front
         (80.0,  40.0),    -- Weak, Far, Front
         (120.0, 10.0),    -- Outside range
         (15.0, 100.0),    -- Right sector
         (15.0, 200.0)     -- Rear sector
        );
   begin
      for I in Test_Cases'Range loop
         R := Detect_Object(Test_Cases(I).Distance, Test_Cases(I).Angle);

         Put_Line("------------------------------");
         Put_Line("Test " & Integer'Image(I));
         Put(" Object Distance: "); 
         Float_IO.Put(Test_Cases(I).Distance, Fore => 3, Aft => 1, Exp => 0);
         Put_Line("ft");
         Put("Object Angle given a quadrant : "); 
         Float_IO.Put(Test_Cases(I).Angle, Fore => 3, Aft => 1, Exp => 0);
         Put_Line("°");
        

         if R.Object_Detected then
            Put_Line("Detected: YES");
            Put_Line("Sector Object Found : " & Radar_Sector'Image(R.Sector));
            Put_Line("Radar Angular Accuracy: " & Radar_Angular_Zone'Image(R.Angular_Zone));
            Put_Line("Radar Range Accuracy : " & Radar_Range_Zone'Image(R.Distance_Zone));
            Put ("Sector Midpoint: ");
            Float_IO.Put(R.Center_Angle, Fore => 3, Aft => 1, Exp => 0);
            Put_Line("°");
            
            Put("True Object Angle from MidPoint : "); 
            Float_IO.Put(R.True_Object_Angle, Fore => 3, Aft => 1, Exp => 0);
            Put_Line("°");


     
        
         else
            Put_Line("Detected: NO");
         end if;
      end loop;
   end;

   Deactivate_Radar(R);
end Test_Radar;





