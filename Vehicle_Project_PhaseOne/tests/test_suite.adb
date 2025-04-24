with AUnit.Test_Suites;
with Test_Vehicle_System;

package body Test_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
      VTC : aliased Test_Vehicle_System.Vehicle_Test_Case;
   begin
      S.Add_Test(VTC'Access);
      return S;
   end Suite;

end Test_Suite;
