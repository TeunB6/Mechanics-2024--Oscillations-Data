require(ggplot2)
require(tidyverse)
require(tools)
require(gsignal)

process_series <- function(path) {}

path = "trial5-resonance2.csv"
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
  
  ggplot(full_data, mapping=aes(x=Time, y=UpperAngle, col=as.factor(Run))) + geom_line(linewidth=1.3)
  ggsave(file.path("plots", paste0(name, "baseplot.png")))
  
  
  # Calculate frequency based on FFT
  calculate_frequency <- function(time, signal, result) {
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
  
  calculate_phase_shift <- function(series1, series2) {
    h1 <- hilbert(series1)
    h2 <- hilbert(series2)
    phase1 <- Arg(h1)
    phase2 <- Arg(h2)
    
    shift = phase1 - phase2
    
    # Put between -pi pi
    phase_shift_wrapped <- (shift + pi) %% (2 * pi) - pi
    return(mean(phase_shift_wrapped))
  }
  
  
  final_data <- full_data %>% 
    summarise(.by = c(Run, Voltage), FrequencyResult = calculate_frequency(Time, UpperAngle),
              FrequencyDrive = calculate_frequency(Time, LowerAngle), MaxAmplitude = max(UpperAngle),
              Phi = calculate_phase_shift(UpperAngle, LowerAngle)) %>%
    summarise(.by = c(Voltage), RFreq = mean(FrequencyResult), FRerr = sd(FrequencyResult),
                DFreq = mean(FrequencyDrive), FDerr = sd(FrequencyDrive),
                MAmpl = mean(MaxAmplitude), MAerr = sd(MaxAmplitude),
                PhaseShift = mean(Phi), PSerr = sd(Phi))
  
  ggplot(final_data, mapping=aes(x=DFreq, y=MAmpl)) +
    geom_point() +
    geom_errorbar(mapping=aes(ymin=MAmpl - MAerr, ymax=MAmpl + MAerr)) +
    labs(x="Driving Frequency (Hz)", y="Amplitude (Rad)") +
    theme_minimal(base_size = 18)
  ggsave(file.path("plots", paste0(name, "FrequencyAmplitude.png")))
  
  ggplot(final_data, mapping=aes(x=DFreq, y=PhaseShift)) +
    geom_point() +
    geom_errorbar(mapping=aes(ymin=PhaseShift - PSerr, ymax=PhaseShift + PSerr)) +
    labs(x="Driving Frequency (Hz)", y="Phase Shift (Rad)") + 
    theme_minimal(base_size = 18)
  ggsave(file.path("plots", paste0(name, "FrequencyPhaseShift.png")))
  
