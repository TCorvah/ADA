package Vehicle_Types is
   -- This package defines the vehicle types used in the vehicle reservation system.
   -- It includes the enumeration of vehicle types and their associated attributes.
   -- The vehicle types are defined as an enumeration type for easy reference and management.
   -- The vehicle types include Standard_cars and Luxury_cars.
   -- The enumeration type is used to categorize the vehicles and manage their attributes.
   -- The vehicle types are used in the Vehicle_Reservation package to manage reservations.
   type Vehicle_Type is (Luxury_cars, Standard_cars);

end Vehicle_Types;