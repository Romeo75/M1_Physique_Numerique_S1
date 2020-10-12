theta0 = 1.
g = 9.81
l = 0.1
m = 6e-3
R = 8.32
n = 8e-7

omega2 = g/l

gamma = 2*n*R/(m*l*l)

Tc = m*g*l/(2*n*R*theta0**2)
T = 0.1*Tc

f(x)=sin(x)/x*(theta0**2-x**2)-gamma/(omega2)*T
df(x)= (theta0**2-x**2)*(x*cos(x)-sin(x))/(x**2) - 2*sin(x)
set xlabel 'tetha (en rad)'
set ylabel 'f(tetha) (en UI)'
plot [-1:1] f(x),df(x),0.