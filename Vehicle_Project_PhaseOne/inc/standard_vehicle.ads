
with Sensor_System, Vehicle_System;
with Vehicle_Types;

package Standard_Vehicle is
   type Door_Status is (close, open);
   type Standard is new Vehicle_System.Vehicle with record
      Car_Sensor : Sensor_System.Sensor;
      Number_Of_Passengers : Integer := 0;
      Model : Vehicle_Types.Vehicle_Type := Vehicle_Types.Standard_cars;
      Cost : Float := 50.0;
      Miles_Per_Gallon : Float := 20.0; 
end record;
  
   procedure SeatBelt_Warning(V : in out Standard);
   procedure Check_Sensors(V : in out Standard);
   function Vehicle_Mobile(V : in out Standard) return Boolean;
   function is_Door_Closed(V : in out Standard) return Boolean;
   

end Standard_Vehicle;


