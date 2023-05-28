using PrecompileTools

@setup_workload begin
  str = "hello world! hello world!"
  strs = [str, "hello", "world"]

  @compile_workload begin
  end
end
