with Road_ProfileConfig; use Road_ProfileConfig;

package Scenario_Type is
   
   -- Define the time of day enumeration
   -- This enumeration defines the possible states of the time of day.
   -- The states include Day and Night, indicating the current visibility conditions.
   -- The enumeration is used to control the sensor system's functionality and behavior.
   -- The time of day is used to determine if the headlights should be turned on or off.
   type Time_of_Day is (Day, Night);

   type Scenario is tagged record
      Time : Time_of_Day;
      Road_Profile : Road_ProfileConfig.Road_Type;
   end record;

   --set the time of day for the scenario
   procedure Set_Time_of_Day(T : in Time_of_Day);

   -- returns the time of day for the scenario
   function Get_Time_of_Day return Time_of_Day;

   -- returns true if night time, false if day time
   function Is_Night return Time_of_Day;

   -- returns true if day time, false if night time
   function Is_Day return Time_of_Day;

   function Get_Road_Profile(V: in Scenario) return Road_ProfileConfig.Road_Type;

   procedure Activate_TOD(Time : in out Scenario);

   end Scenario_Type;
