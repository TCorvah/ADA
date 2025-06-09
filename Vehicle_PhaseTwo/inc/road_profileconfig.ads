package Road_ProfileConfig is

   type  Road_Type is (Garage, Country_Road, Busy_City, Highway);

   type Road_Profile is record
      Name : Road_Type;
      Max_Speed : Float;
      Min_Speed : Float;
      Speed_Limit : Float;
      --Traffic_Density : Float;
      --Weather_Condition : Float;
   end record;



end Road_ProfileConfig;
