pressure_plot <- function(maxyear) {
  acd_mean_plot <- ggplot(data = acid_ts, aes(x = year, y = acidification)) +
    geom_line() +
    geom_ribbon(aes(ymin = acidification - sd, ymax = acidification + sd),
                fill = "black", alpha = 0.15) +
    theme_classic() +
    labs(x = "Year", y = "pH")

  sla_mean_plot <- ggplot() +
    geom_line(data = sla_ts, aes(x = year, y = sla, color = season)) +
    geom_ribbon(data = sla_ts,aes(x = year, y = sla,
                                  ymin = sla - sd,
                                  ymax = sla + sd, fill = season),
                alpha = 0.2, show.legend = F) +
    theme_classic() +
    labs(x = "Year", y = "SLA (m)", color = "Season") +
    scale_color_manual(labels = c("Summer","Winter"), values = c("#e1ad68", "#256b61"))+
    scale_fill_manual(labels = c("Summer","Winter"), values = c("#e1ad68", "#256b61"))

  sst_mean_plot <- ggplot() +
    geom_line(data = sst_ts, aes(x = year, y = sst, color = season)) +
    geom_ribbon(data = sst_ts,aes(x = year, y = sst,
                                  ymin = sst - sd,
                                  ymax = sst + sd, fill = season),
                alpha = 0.2, show.legend = F) +
    theme_classic() +
    labs(x = "Year", y = "SST (°C)", color = "Season")+
    scale_color_manual(labels = c("Summer", "Winter"), values = c("#e1ad68", "#256b61"))+
    scale_fill_manual(labels = c("Summer", "Winter"), values = c("#e1ad68", "#256b61"))

  dhw_mean_plot <- ggplot() +
    geom_vline(xintercept = maxyear[1], color = "red", linetype = 5, alpha = 0.5) +
    geom_vline(xintercept = maxyear[2], color = "red", linetype = 5, alpha = 0.5) +
    geom_line(data = dhw_ts, aes(x = year, y = dhw)) +
    geom_ribbon(data = dhw_ts,aes(x = year, y = dhw,
                                  ymin = dhw - sd,
                                  ymax = dhw + sd),
                alpha = 0.2, show.legend = F) +
    theme_classic() +
    labs(x = "Year", y = "DHW (°C/weeks)")
  acd_mean_plot / sla_mean_plot / sst_mean_plot / dhw_mean_plot

}
