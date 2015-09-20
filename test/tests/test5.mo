block test5
  input Real in0;
  input Real in1;
  input Real x;
  input Boolean positiveVelocity;
protected
  Real points[:];
  Real values[:];
  Real x0;
  Integer m;
  Real y;
algorithm

  when true then
    if x>x0 then
      m:=size(points, 1);
    elseif x<x0 then
      while true loop
	m:=m+1;
      end while;

    end if;

    x0:=x;
  end when;

end test5;

