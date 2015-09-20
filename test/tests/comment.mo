block comment
  input Real in0;
  input Real in1;
  input Real x;
  input Boolean positiveVelocity;
protected
  Real points[:];
  Real values[:]; // comment
  Real x0;
  Integer /* comment */ m;
algorithm

  when true then
    if x>x0 then
      m:=size(points, 1);
    elseif x<x0 then

// comment
// comment

      while true loop
	m:=m+1;
/* comment
comment */
      end while;

    end if;

    x0:=x;
  end when;

end comment;

