library(xts)
load('ts_fdom_discharge.Rdata')

ts = tscombined$Discharge
times = index(ts)
tsfilled <-merge(ts, zoo(, seq(min(times), max(times), "min")))


#ts = ?

ts = fdom_corrected_daily_lognormal_filled;
fdom = ts(:,2);
L = length(fdom);
baseflow = nan(L,1);
window = 11;
last_minima = nan;
for i = 1:L
hw = floor(window/2);
if i <=  hw
baseflow(i) = nan;
continue;
end
if i >= L - hw
baseflow(i) = nan;
continue;
end 
minima = min(fdom(i-hw:i+hw));
if fdom(i) == minima
baseflow(i) = fdom(i);
elseif fdom(i) < last_minima
%baseflow(i) = fdom(i);
end
if ~isnan(baseflow(i))
%last_minima = baseflow(i);
end

end