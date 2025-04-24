with Ada.Text_IO; use Ada.Text_IO;
with Sensor_System, Vehicle_Systems;

package body Standard_Vehicle is 


procedure SeatBelt_Warning(V : in out  Standard) is
begin
  Sensor_System.Activate(V.Sensors);
   Sensor_System.Check_Seat(V.Sensors);
   if Sensor_System.Seat_Occupied (V.Sensors) and then not V.Sensors.Seatbelt_On then 
      Put_Line(" seatbelt is not on");
   end if;  
end SeatBelt_Warning;

procedure Check_Sensors(V : in Standard) is   
begin
   Sensor_System.Activate_Sensor(V.Sensors);
   if Sensor_System.Is_Door_Open(V.Sensors) then
      Put_Line(" Door is open");
   else
       Put_Line(" Door is closed");
   end if;
   Sensor_System.Check_Seat(V.Sensors);
end Check_Sensors;

end Standard_Vehicle;