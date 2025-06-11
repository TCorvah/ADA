with Vehicle_Types; use Vehicle_Types;
with Road_ProfileConfig; use Road_ProfileConfig;
package Vehicle_Constants is

   -- Constants for vehicle types
   Luxury_Rental_Cost : constant Float := 100.0; -- Cost per day for luxury rental    
   Standard_Rental_Cost : constant Float := 50.0; -- Cost per day for standard rental

   Luxury_MPG : constant Float := 20.0; -- Miles per gallon for luxury vehicle
   Standard_MPG : constant Float := 30.0; -- Miles per gallon for standard vehicle
   Threshold : constant Float := 10.0;
   Minimum_Weight_Occupancy : constant Float := 20.0; -- Minimum weight for occupancy check
   Minimum_Constant_Distance : constant Float := 10.0; -- Minimum speed for vehicle to be considered mobile
   MAX_Speed : constant Float := 45.0; -- Minimum speed for vehicle to be considered mobile
   Min_Caution_Distance : constant Float := 30.0; -- Minimum caution distance for vehicle to be considered mobile
   half_Constant_Float : constant Float := 0.5; -- Half of a constant float value
   Full_Circle_Angle : constant Float := 360.0; -- Full circle angle in degrees
   Vehicle_Max_Speed : constant Float := 60.0; -- Maximum speed for any vehicle
   Vehicle_Min_Speed : constant Float := 5.0; -- Minimum speed for any vehicle

end Vehicle_Constants;