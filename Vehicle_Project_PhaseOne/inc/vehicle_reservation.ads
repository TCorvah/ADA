
package Vehicle_Reservation is

type Vehicle_Type is (Standard, Luxury);

type Reservation is record
   Name         : String (1 .. 50);
   Credit_Card  : String (1 .. 10);
   Car_Type     : Vehicle_Type;
end record;

procedure Input_Driver_Name(Reservation : in out Reservation);
procedure Input_Credit_Card(Reservation : in out Reservation);
procedure Input_Car_Type(Reservation : in out Reservation);
procedure Display_Reservation(Reservation : in Reservation);
procedure Confirm_Reservation(Reservation : in Reservation);
procedure Cancel_Reservation(Reservation : in out Reservation);


end Vehicle_Reservation;
