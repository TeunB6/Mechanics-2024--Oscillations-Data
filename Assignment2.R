require(ggplot2)
require(tidyverse)
require(tools)
require(gsignal)
require(zoo)

path = "trial2-calibratemagnet.csv"
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
  mutate(LowerAngle = (LowerAngle - mean(LowerAngle))) %>%
  mutate(UpperAngle = (UpperAngle - mean(UpperAngle))) %>%
  ungroup()

# Remove leading 0
full_data <- full_data %>%
  group_by(Run) %>%
  dplyr::filter(row_number() >= which.max(abs(UpperAngle) < 0.01 | abs(LowerAngle) < 0.01)) %>% # Keep from first non-zero value
  ungroup()

ggplot(full_data, mapping=aes(x=Time, y=UpperAngle, col=as.factor(Run))) + geom_line(linewidth=2)
ggsave(file.path("plots", paste0(name, "baseplot.png")))


get_amplitude <- function(y) {
  analytic <- hilbert(y)
  return(Mod(analytic))
}

# Running average function
compute_running_average <- function(x, window_size) {
  rollapply(x, width = window_size, FUN = mean, align = "center", fill = NA)
}

final_data <- full_data %>% group_by(Run) %>%
              mutate(Amplitude = get_amplitude(UpperAngle)) %>%
              mutate(AvgAmplitude = compute_running_average(Amplitude, 25)) %>%
              ungroup()

ggplot(final_data %>% dplyr::filter(Run == 3)) + 
  geom_line(mapping=aes(x=Time, y=UpperAngle), color='blue') +
  geom_point(mapping=aes(x=Time, y=AvgAmplitude), color="red")


Oscilation <- seq(1,10,length.out=10)
Amplitude2 <- c(4.79,4.07,3.46,2.92,2.41,1.94,1.47,1.056,0.56,0.26)
Amplitude3 <- c(6.67,5.73,4.58,3.69,2.99,2.36,1.84,1.40,0.89,0.39)
Amplitude4 <- c(7.87,5.95,4.99,4.12,3.26,2.44,1.81,1.29,0.70,0.57)
amplitude_data <- data.frame(Oscilation, Amplitude2, Amplitude3, Amplitude4) %>%
  summarise(.by=Oscilation, Amplitude=mean(c(Amplitude4,Amplitude3,Amplitude2)), AmplError = sd(c(Amplitude4,Amplitude3,Amplitude2)))

fitted <- lm(log(Amplitude,10) ~ Oscilation, amplitude_data)


ggplot(amplitude_data) + 
  geom_point(mapping=aes(x=as.factor(Oscilation), y=Amplitude), color="royalblue") +
  geom_line(mapping=aes(x=Oscilation, y=10^fitted$fitted.values), color="#C8102E", linewidth=1.5) +
  geom_errorbar(mapping=aes(x=Oscilation, ymin=Amplitude - AmplError,ymax=Amplitude + AmplError), width=0.5, col= "royalblue") +
  scale_y_log10() +
  theme_minimal(base_size = 20) +
  labs(x="Oscillation", y="Amplitude (Rad)", title="Amplitude for each Oscillation on a Logarithmic Scale")
ggsave(file.path("plots", paste0(name, "_amplitude.png")))