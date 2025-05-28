

# 🚗 Vehicle Rental Simulation (Ada Embedded System) – Phase Two

## 📘 Overview

**Vehicle Rental Simulation** is a self-developed Ada project simulating a real-time vehicle rental platform with embedded system behavior modeling. The simulation includes **Standard** and **Luxury** vehicles, each integrated with subsystems that emulate practical sensor, radar, and control logic.

This project began as a platform to explore **Ada** and real-time system design. Phase Two introduces expanded functionality with:
- Smarter **timing models**
- **Randomized sensor and radar events**
- Scenario logic tailored for practical environments like **parking garages**
- Elimination of redundant user input for enhanced realism

---

## 🔧 Subsystem Components

| Module Name                | Description                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| `vehicle`                  | Abstract parent type for all vehicles (base attributes & behaviors)         |
| `standard`                 | Inherits from `vehicle`; basic sensors, relies more on user input           |
| `luxury`                   | Inherits from `vehicle`; integrates radar and advanced automated features   |
| `sensor_system`            | Monitors seat occupancy, visibility, door states, etc.                      |
| `radar`                    | Simulates proximity detection and rear-view collision avoidance             |
| `timing_controller`        | Introduces time-of-day context, affecting sensor behavior and visibility    |
| `system_interface`         | Manages interaction between scenario triggers and vehicle components        |
| `vehicle_reservation`      | Handles reservation logic and scenario initiation                           |
| `main`                     | Central simulation engine coordinating all modules and scenarios            |

---

## 🆕 Phase Two Upgrades

### ✅ Refined Scenario Logic
- **Occupancy weight is now implicit**: if the door opens → closes → engine starts → movement is attempted, the system assumes the driver is present and weighs at least **20kg**.
- This removes the need for manual weight input, increasing realism.

### 🏙️ Environment-Sensitive Simulation
- If the scenario occurs in a **parking garage**:
  - Forward **line of sight** is limited to **±45°**
  - Rear radar detection covers **270°**
  - This helps simulate tight-space movement and collision prevention logic.

### 🎲 Randomization vs. User Control (Design Notes)
- Current design is evolving to balance **user input (Standard vehicle)** and **randomized behaviors (Luxury vehicle)**:
  - **Standard vehicle**: Requires more user-driven configuration to reflect minimal automation.
  - **Luxury vehicle**: Behaviors are increasingly randomized to simulate advanced embedded intelligence.
- Scenarios may include:
  - Random sensor anomalies
  - Timing fluctuations (e.g., delays between door open and engine start)
  - Unpredictable field of view shifts due to lighting/time of day

> ⚠️ **Design Question (In Progress):** Should future scenarios become **entirely random**, or should Standard vehicles retain more manual control?  
> **Open issue** for feedback and testing.

---

## 🛠 Technologies & Methods

- Language: **Ada** (GNAT)
- Concepts:
  - Tagged types and type extension
  - Real-time modeling
  - Sensor fusion and radar logic
  - Environment-driven decision-making
- Build Tool: `gprbuild`


## 📂 Directory Structure
Vehicle_PhaseTwo/
├── src/
│ ├── main.adb
│ ├── vehicle.ads/adb
│ ├── standard.ads/adb
│ ├── luxury.ads/adb
│ ├── sensor_system.ads/adb
│ ├── radar.ads/adb
│ ├── timing_controller.ads/adb
│ ├── system_interface.ads/adb
│ ├── vehicle_reservation.ads/adb
├── test/
│ 
└── README.txt


