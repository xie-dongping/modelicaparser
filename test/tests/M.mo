model M
  constant Real c = 5;

  model Foo
    parameter Real p = 3;
    Real x;
  equation
    x = p * sin(time) + c;
  end Foo;

  Foo f(p = 17);

equation
  f. x + f.x = 12;
  f .x .+ 8 = 18;

end M;