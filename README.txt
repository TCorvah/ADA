Vehicle Simulation System – Phase One

📌 Overview

This project is a modular vehicle simulation written in Ada, demonstrating core embedded systems concepts such as sensor checks, radar logic, user interaction, and control flow.

Users can:

Select a Standard or Luxury vehicle
Simulate a test drive based on time of day and scenario
Make or cancel a reservation, with rental cost and MPG displayed

✅ Features Implemented in Phase One

🚗 Vehicle Selection
Standard and Luxury car types available
Default rental cost and MPG are associated with each type
🎮 Simulation Interface
User selects a scenario (e.g., parking garage)
Simulation includes:
Door sensor
Seatbelt sensor
Engine start sequence
Radar detection (Luxury only)
Automatic headlights (based on time of day)
🌙 Time of Day Selection
Night mode activates automatic headlights
⚠️ Safety Checks
Luxury car:
Will not move if the seatbelt is unfastened or an object is detected nearby
Standard car:
Gives warnings but still allows movement
📝 Reservation System
After simulation, users can enter:
Their name
A 10-digit credit card number
Option to confirm or cancel the reservation

📁 Project Structure

| File                        | Description                                                      |
| --------------------------- | ---------------------------------------------------------------- |
| `main.adb`                  | Entry point – ties all modules together                          |
| `vehicle.ads/.adb`          | Base vehicle type and shared behaviors                           |
| `luxury_car.ads/.adb`       | Logic for luxury vehicle (radar, safety sensors)                 |
| `standard_car.ads/.adb`     | Logic for standard vehicle (basic sensors only)                  |
| `system_interface.ads/.adb` | Menu navigation, scenario selection, simulation flow             |
| `reservation.ads/.adb`      | Handles name/credit input, reservation confirmation/cancellation |
| `vehicle_constants.ads`     | Contains rental cost and MPG constants for vehicle types         |

🔧 Technologies Used

Language: Ada

Concepts:
Object-Oriented Programming (OOP)
Encapsulation
Polymorphism
Constants and Type Declarations
Case Statements
User Input Validation


🔮 Future Phases

Phase Two(Currently working on):
Simulated timers
Tasking for radar polling
Protected shared data
Advanced simulation logic


Phase Three(In the future):
UART interface simulation
Integration of external sensors
Implementation of Hardware Abstraction Layer (HAL)


🚀 How to Run

1. Ensure GNAT and GPRbuild are installed
    You can install them via Alire or your system's package manager.
2. Build the Project
In the root directory of the project (where test_system.gpr is located), run:

    gprbuild -P test_system.gpr

3. Run the Executable
The executable is typically placed in the obj/ directory (as specified in the .gpr file). Run it like this:

./obj/main
