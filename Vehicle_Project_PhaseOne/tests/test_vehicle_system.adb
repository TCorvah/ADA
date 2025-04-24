with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;
with Vehicle_System;
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Vehicle_System is

   overriding
   procedure Run (T : in out Vehicle_Test_Case) is
      V : Vehicle_System.Vehicle;
   begin
      -- Test Start_Engine
      Vehicle_System.Start_Engine(V);
      Assert(V.Engine_On, "Engine should be on after starting.");

      -- Test Set_Speed when vehicle is not moving
      Vehicle_System.Set_Speed(V, 50);
      Assert(V.Speed = 0.0, "Speed should remain 0 when vehicle is not moving.");

      -- Set vehicle to moving and test Set_Speed
      V.Is_Moving := True;
      Vehicle_System.Set_Speed(V, 60);
      Assert(V.Speed = 60.0, "Speed should be set to 60 when vehicle is moving.");

      -- Test Stop_Engine
      Vehicle_System.Stop_Engine(V);
      Assert(not V.Engine_On, "Engine should be off after stopping.");
      Assert(V.Speed = 0.0, "Speed should be 0 after stopping the engine.");
   end Run;

end Test_Vehicle_System;
