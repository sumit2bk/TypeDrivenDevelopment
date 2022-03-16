data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
  Bicycle: Vehicle Pedal
  Car: (fuel: Nat) -> Vehicle Petrol
  Bus: (fuel: Nat) -> Vehicle Petrol

||| As you can see here using generic type syntax you can't define dependent types really the way
||| you did in above case.
data Vehicle2 source = Bicycle2 source


wheels: Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4


refuel: Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Car 200
