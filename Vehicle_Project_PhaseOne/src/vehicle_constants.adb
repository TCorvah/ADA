with Vehicle_Types; use Vehicle_Types;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Standard_Vehicle; use Standard_Vehicle;
with Luxury_Vehicle; use Luxury_Vehicle;
package body Vehicle_Constants is


   function Get_Rental_Cost(VT: Vehicle_Types.Vehicle_Type) return Float is
   begin
      case VT is
         when Vehicle_Types.Luxury_cars =>
            return Luxury_Rental_Cost;
         when Vehicle_Types.Standard_cars =>
            return Standard_Rental_Cost;
         when others =>
            Put_Line("Invalid vehicle type.");
            return 0.0;
      end case;
   end Get_Rental_Cost;

   function Get_MPG(VT: Vehicle_Types.Vehicle_Type) return Float is
   begin
      case VT is
         when Vehicle_Types.Luxury_cars =>
            return Luxury_MPG;
         when Vehicle_Types.Standard_cars =>
            return Standard_MPG;
      end case;
   end Get_MPG;


end Vehicle_Constants;