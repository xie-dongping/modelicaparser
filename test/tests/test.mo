model Test



function func

  input Real a;

  input Real b;

  output Real c;

  output Real d;

algorithm

  c := a*a;

  d := b+b;

end func;



  parameter Real e = 3;

  parameter Real f = 5;

  Real g;

equation

  (,g) = func(e,f);

end Test;
