require(ggplot2)
require(tidyverse)
require(tools)
require(gsignal)

path = "trial1-hand.csv"
name = file_path_sans_ext(path)
data <- read.csv(path, sep = ';')
full_data <- data %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(Run = as.integer(str_extract(name, "(\\d+$)"))) %>%
  mutate(name = str_extract(name, "^(.*?)(?=Run\\.\\.\\d+$)")) %>%
  pivot_wider(id_expand = T, names_from = name, values_from = value, values_fn = list) %>%
  unnest(!contains("Run")) %>%
  select(c(Angle..Ch.1.2..rad.., Angle..Ch.3.4..rad.., Time..s.., Run, Output.Voltage..V..)) %>%
  rename("UpperAngle" = Angle..Ch.1.2..rad.., "LowerAngle" = Angle..Ch.3.4..rad.., "Time" = Time..s.., "Voltage" = Output.Voltage..V..) %>%
  drop_na() %>%
  mutate(LowerAngle = str_replace_all(LowerAngle, ",", "."),
         UpperAngle = str_replace_all(UpperAngle, ",", "."),
         Time = str_replace_all(Time, ",", "."),
         Voltage = str_replace_all(Voltage, ",", ".")) %>%
  mutate(LowerAngle = as.numeric(LowerAngle), UpperAngle = as.numeric(UpperAngle),
         Time = as.numeric(Time), Voltage = as.numeric(Voltage),Run = as.numeric(Run)) %>%
  mutate(Voltage = round(Voltage, 1)) %>%
  drop_na()

# Translate to Equilibrium = 0
full_data <- full_data %>% group_by(Run) %>%
  mutate(LowerAngle = LowerAngle - mean(LowerAngle)) %>%
  mutate(UpperAngle = UpperAngle - mean(UpperAngle)) %>%
  ungroup()

# Calculate frequency based on FFT
calculate_frequency <- function(time, signal) {
  # Perform a Fourier transform to find the frequency
  fft_result <- fft(signal)
  
  # Compute the frequency axis
  n <- length(signal)
  sample_rate <- 1 / mean(diff(time))  # Assuming evenly spaced data
  freq_axis <- seq(0, sample_rate / 2, length.out = n / 2)
  
  # Take the absolute value of the fft and identify the peak frequency
  magnitude <- Mod(fft_result)
  peak_freq <- freq_axis[which.max(magnitude[1:(n/2)])]  # Ignore the negative frequencies
  return(peak_freq)
}

ggplot(full_data, mapping=aes(x=Time, y=UpperAngle, col=as.factor(Run))) + geom_line(linewidth=2)
ggsave(file.path("plots", paste0(name, "baseplot.png")))


freq <- full_data %>% summarise(.by=c(Run), freq=(calculate_frequency(Time, UpperAngle)))
frequencies <- mean(freq$freq)
freq_err <- sd(freq$freq)

             