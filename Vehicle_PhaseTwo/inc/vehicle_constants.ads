with Vehicle_Types; use Vehicle_Types;
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
end Vehicle_Constants;