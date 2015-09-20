
model MotorDrive
  PI controller;
  Motor motor;
  Gearbox gearbox(n=100);
  Shaft Jl(J=10);
  Tachometer wl;
equation
  connect(controller.out, motor.inp);
  connect(motor.flange , gearbox.a);
  connect(gearbox.b , Jl.a);
  connect(Jl.b , wl.a);
  connect(wl.w , controller.inp);
end MotorDrive;