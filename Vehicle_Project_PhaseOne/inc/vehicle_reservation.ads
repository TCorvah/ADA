
package Vehicle_Reservation is

type Vehicle_Type is (Standard, Luxury);

type Reservation is record
   Name         : String (1 .. 50);
   Credit_Card  : String (1 .. 10);
   Car_Type     : Vehicle_Type;
   Rental_Fee   : Float;
   MPG          : Float;
end record;


end Vehicle_Reservation;
