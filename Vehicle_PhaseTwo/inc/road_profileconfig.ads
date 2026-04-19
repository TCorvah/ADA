with Vehicle_Constants; Use Vehicle_Constants;

package Road_ProfileConfig is

   type Road_Type is (Garage, Country_Road, Busy_City, Highway);
   type Road_Profile is record
      Name : Road_Type;
      Max_Speed : Float;
      Min_Speed : Float;
      Speed_Limit : Float;
      --Min_Distance : Float;
      --Max_Distance : Float;
      Caution_Distance : Float;
      Max_Detection_Range : Float;
      Min_Detection_Range : Float;
    
      --Traffic_Density : Float;
      --Weather_Condition : Float;
   end record;
 
   -- Predefined road profiles:
   Garage_Profile : constant Road_Profile := (
      Name        => Garage,
      Max_Speed   => 20.0,
      Min_Speed   => 5.0,
      Speed_Limit => 10.0,
      Min_Detection_Range => 2.0,
      Caution_Distance => 10.0,
      Max_Detection_Range => 20.0
   );

   City_Profile : constant Road_Profile := (
      Name        => Busy_City,
      Max_Speed   => 40.0,
      Min_Speed   => 20.0,
      Speed_Limit => 35.0,
      Min_Detection_Range => 2.0,
      Caution_Distance => 5.0,
      Max_Detection_Range => 50.0
   );

   Country_Profile : constant Road_Profile := (
      Name => Country_Road,
      Max_Speed => 50.0,
      Min_Speed   => 20.0,
      Speed_Limit => 50.0,
      Min_Detection_Range => 2.0,
      Caution_Distance => 10.0,
      Max_Detection_Range => 60.0
   );

   Highway_Profile : constant Road_Profile := (
      Name        => Highway,
      Max_Speed   => 75.0,
      Min_Speed   => 40.0,
      Speed_Limit => 75.0,
      Min_Detection_Range => 2.0,
      Caution_Distance => 60.0,
      Max_Detection_Range => 120.0
   );

end Road_ProfileConfig;




