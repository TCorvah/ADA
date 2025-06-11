with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   
with Road_ProfileConfig;  use Road_ProfileConfig;

procedure Test_Road_Profile is
   package Float_IO is new Ada.Text_IO.Float_IO(Float);
begin
   Put_Line("Testing Road Profile Constants:");

   Put("Garage Scenario Max Speed: ");
   Float_IO.Put(Garage_Profile.Max_Speed, Fore => 3, Aft => 1, Exp => 0);
   Put_Line(" km/h");

   Put("Highway Scenario Speed Limit: ");
   Float_IO.Put(Highway_Profile.Speed_Limit, Fore => 3, Aft => 1, Exp => 0);
   Put_Line(" km/h");

   Put("Country Road Scenario Min Speed: ");
   Float_IO.Put(Country_Profile.Min_Speed, Fore => 3, Aft => 1, Exp => 0);
   Put_Line(" km/h");
end Test_Road_Profile;
