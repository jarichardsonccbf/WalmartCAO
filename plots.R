# Plots ----

library(patchwork)

time.avg.p <- spring.visit.all %>%
  group_by(CAO) %>%
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T),
            count = n()) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = as.numeric(`Average minutes in outlet`), fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = `Average minutes in outlet` - SD, ymax = `Average minutes in outlet` + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average Time in Outlet (minutes)") +
  theme_bw() +
  geom_text(aes(label = count, y = `Average minutes in outlet`), hjust = 5.25, vjust = 1.1)

time.total.p <- spring.visit.all %>%
  group_by(CAO) %>%
  summarise(total.hrs = sum(Actual.Time..Minutes., na.rm = T),
            count = n()) %>%
  mutate(total.hrs = total.hrs / 60,
         CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = as.numeric(total.hrs), fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total Hours in Outlets") +
  theme_bw() +
  geom_text(aes(label = count, y = total.hrs), hjust = 5.25, vjust = 1.1)

time.dc.avg.p <- spring.visit.all %>%
  group_by(DC, CAO) %>%
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T),
            count = n()) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = as.numeric(`Average minutes in outlet`), fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = `Average minutes in outlet` - SD, ymax = `Average minutes in outlet` + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average Time in Outlet (minutes)") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(aes(label = count, y = `Average minutes in outlet`), hjust = 1.25, vjust = 1.01)

time.dc.total.p <- spring.visit.all %>%
  group_by(DC, CAO) %>%
  summarise(total.hrs = sum(Actual.Time..Minutes., na.rm = T),
            count = n()) %>%
  mutate(total.hrs = total.hrs / 60,
         CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = as.numeric(total.hrs), fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total Hours in Outlets") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(aes(label = count, y = total.hrs), hjust = 1.25, vjust = 1.01)

(time.avg.p / time.total.p) | (time.dc.avg.p / time.dc.total.p)


# financials ----

# volume

volume.avg.p <- spring.visit.perf %>%
  group_by(CAO) %>%
  summarise(mean = mean(Volume, na.rm = T),
            SD = sd(Volume, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = mean, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average Volume") +
  theme_bw()

volume.avg.p

volume.total.p <- spring.visit.perf %>%
  group_by(CAO) %>%
  summarise(tote = sum(Volume, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = tote, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total Volume") +
  theme_bw()

volume.total.p

volume.dc.avg.p <- spring.visit.perf %>%
  group_by(DC, CAO) %>%
  summarise(mean = mean(Volume, na.rm = T),
            SD = sd(Volume, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = mean, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average Volume") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

volume.dc.avg.p

volume.dc.total.p <- spring.visit.perf %>%
  group_by(DC, CAO) %>%
  summarise(mean = sum(Volume, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = mean, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total Volume") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

volume.dc.total.p

(volume.avg.p / volume.total.p) | (volume.dc.avg.p / volume.dc.total.p)

# Dead Net Revenue

dnr.avg.p <- spring.visit.perf %>%
  group_by(CAO) %>%
  summarise(mean = mean(`Dead Net Revenue`, na.rm = T),
            SD = sd(`Dead Net Revenue`, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = mean, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average DNR") +
  theme_bw()

dnr.avg.p

dnr.total.p <- spring.visit.perf %>%
  group_by(CAO) %>%
  summarise(tote = sum(`Dead Net Revenue`, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = tote, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total DNR") +
  theme_bw()

dnr.total.p

dnr.dc.avg.p <- spring.visit.perf %>%
  group_by(DC, CAO) %>%
  summarise(mean = mean(`Dead Net Revenue`, na.rm = T),
            SD = sd(`Dead Net Revenue`, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = mean, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average DNR") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

dnr.dc.avg.p

dnr.dc.total.p <- spring.visit.perf %>%
  group_by(DC, CAO) %>%
  summarise(mean = sum(`Dead Net Revenue`, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = mean, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total DNR") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

dnr.dc.total.p

(dnr.avg.p / dnr.total.p) | (dnr.dc.avg.p / dnr.dc.total.p)

# Dead Net Gross Profit

dngp.avg.p <- spring.visit.perf %>%
  group_by(CAO) %>%
  summarise(mean = mean(`Dead Net Gross Profit`, na.rm = T),
            SD = sd(`Dead Net Gross Profit`, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = mean, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average DNGP") +
  theme_bw()

dngp.avg.p

dngp.total.p <- spring.visit.perf %>%
  group_by(CAO) %>%
  summarise(tote = sum(`Dead Net Gross Profit`, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = tote, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total DNGP") +
  theme_bw()

dngp.total.p

dngp.dc.avg.p <- spring.visit.perf %>%
  group_by(DC, CAO) %>%
  summarise(mean = mean(`Dead Net Gross Profit`, na.rm = T),
            SD = sd(`Dead Net Gross Profit`, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = mean, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average DNGP") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

dngp.dc.avg.p

dngp.dc.total.p <- spring.visit.perf %>%
  group_by(DC, CAO) %>%
  summarise(mean = sum(`Dead Net Gross Profit`, na.rm = T)) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = mean, fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total DNGP") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

dngp.dc.total.p

(dngp.avg.p / dngp.total.p) | (dngp.dc.avg.p / dngp.dc.total.p)
