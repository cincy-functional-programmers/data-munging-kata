defmodule WeatherReport do
  @temperature_dataregex ~r/^\s*(\d+)\s+(\d+)[\s\*]+(\d+)/

  def main(data_file) do
    result = DataExtract.find_smallest_diff(data_file, @temperature_dataregex, &extract_temp_data/1)
    IO.puts(result)
  end

  defp extract_temp_data([_, day, max, min]) do
    {day, max, min}
  end
end
