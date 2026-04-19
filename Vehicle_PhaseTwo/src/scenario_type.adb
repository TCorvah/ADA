
package body Scenario_Type is

   procedure Set_Time_of_Day(T : in Time_of_Day) is
   begin
      Time := T;
   end Set_Time_of_Day;

   function Get_Time_of_Day return Time_of_Day is
   begin
      return Time;
   end Get_Time_of_Day;
   

   function Is_Night return Time_of_Day is
   begin
      return Time = Night;
   end Is_Night;

   function Is_Day return Time_of_Day is
   begin
      return Time = Day;
   end Is_Day;

   function Get_Road_Profile(V: in Scenario) return Road_ProfileConfig.Road_Type is
   begin
      return V.Road_Profile;
   end Get_Road_Profile;

   -- Procedure to activate the time of day
   -- This procedure prompts the user for the time of day and sets the visibility accordingly.
   -- It takes a Sensor object as an input parameter and modifies its visibility attribute.
   -- The procedure also prints a message indicating the time of day.
   -- The procedure is designed to be used in conjunction with the Sensor_System package.
   procedure Activate_TOD(Time : in out Scenario) is
   begin
     Scenario.Set_Time_of_Day(Time);
   end Activate_TOD;

  
end Scenario_Type;