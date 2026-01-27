

package Vehicle_Constants is

   -- Constants for vehicle types
   Luxury_Rental_Cost : constant Float := 100.0; -- Cost per day for luxury rental    
   Standard_Rental_Cost : constant Float := 50.0; -- Cost per day for standard rental

   Luxury_MPG : constant Float := 20.0; -- Miles per gallon for luxury vehicle
   Standard_MPG : constant Float := 30.0; -- Miles per gallon for standard vehicle

   Threshold : constant Float := 10.0;

   Minimum_Weight_Occupancy : constant Float := 90.0; -- Minimum weight for occupancy to determine if a vehicle is occupied


   half_Constant_Float : constant Float := 0.5; -- Half of a constant float value

  
   Vehicle_Max_Speed : constant Float := 100.0; -- Maximum speed for any vehicle
   Vehicle_Min_Speed : constant Float := 5.0; -- Minimum speed for any vehicle to be considered moving

   Full_Circle_Angle : constant Float := 360.0; -- Full circle angle in degrees

   Number_of_Sectors : constant Integer := 4; -- Number of radar sectors (front, right, rear, left)


   -- Minimum distance (in feet) at which the radar can reliably detect an object.
   -- Objects closer than this are considered sensor blind spots but can easily be seen by the driver.
   Minimum_Detection_Range : constant Float := 2.0;

   -- Maximum distance (in feet) at which the radar can reliably detect an object.
   -- Objects beyond this distance are outside radar sensing capability and maybe consider clear as no 
   -- immediate threat to vehicle safety detected.
   Maximum_Detection_Range : constant Float := 100.0;

   Min_Caution_Range : constant Float := 5.0; -- Minimum distance to trigger caution alert


   
   -- Field of view angle for each radar sector (in degrees).
   -- Each sector covers a 90-degree quadrant of the 360-degree field of view.
   Angle_Sector_FOV : constant Float := Full_Circle_Angle / Float(Number_of_Sectors);

   -- Midpoints of each radar sector (in degrees).
   -- These midpoints represent the center angles of the front, right, rear, and left
   Front_Sector_Midpoint : constant Float := 0.0; -- 0 degrees for front sector
   Right_Sector_Midpoint : constant Float := 90.0; -- 90 degrees for right sector
   Rear_Sector_Midpoint : constant Float := 180.0; -- 180 degrees for rear sector
   Left_Sector_Midpoint : constant Float := 270.0;  -- 270 degrees for left sector


-- Maximum allowable angular deviation (in degrees) from a sector's midpoint.
-- This value represents half of the sector field-of-view (90° FOV → 45°).
-- A radar object is detected within a sector if its angle falls within this deviation
-- from the sector's midpoint.
Detection_Angle : constant Float := Angle_Sector_FOV / 2.0;



   ---------------------------------------
   --Radar Angle Geometry
   --------------------------------------


   -- Precomputed Sector Angles for Radar Detection using 90-degree sectors (front centered midpoint at 0 degrees)
   Front_Sector_Start : constant Float := Full_Circle_Angle - Detection_Angle; -- 315 degrees for start of front sector
   Front_Sector_End : constant Float := Front_Sector_Midpoint + Detection_Angle; -- 45 degrees for end of front sector
   Right_Sector_Start : constant Float := Right_Sector_Midpoint - Detection_Angle; -- 45 degrees for start of right sector
   Right_Sector_End : constant Float :=  Right_Sector_Midpoint + Detection_Angle; --135 degrees for end of right sector
   Rear_Sector_Start : constant Float := Rear_Sector_Midpoint - Detection_Angle; -- 135 degrees for start of back sector
   Rear_Sector_End : constant Float :=  Rear_Sector_Midpoint +  Detection_Angle; -- 225 degrees for end of back sector
   Left_Sector_Start : constant Float := Left_Sector_Midpoint - Detection_Angle; -- 225 degrees for start of left sector
   Left_Sector_End : constant Float := Left_Sector_Midpoint + Detection_Angle; -- 315 degrees for end of left sector

end Vehicle_Constants;