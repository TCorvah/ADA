with AUnit.Run;
with Test_Suite;

procedure Test_Runner is
begin
   AUnit.Run.Test_Suite (Test_Suite.Suite);
end Test_Runner;
