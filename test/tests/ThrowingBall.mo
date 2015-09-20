model ThrowingBall
  Real x(start=0);
  Real y(start=1);

  equation

    der(x)=2*x;
    der(y)=5*y;

  algorithm

    when y<0 then
      terminate("The ball touches the ground");
    end when;


end ThrowingBall;