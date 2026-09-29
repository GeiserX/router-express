<p align="center">
  <img src="docs/images/banner.svg" alt="Router Express" />
</p>

<h1 align="center">Router Express</h1>

<p align="center">
  <a href="LICENSE"><img src="https://img.shields.io/github/license/GeiserX/router-express" alt="License" /></a>
</p>

<p align="center">Auto-configuration of an ISP's client routers (Ethernet, ADSL, voice) over DHCP option 66 and TR-069</p>

---

An R/Shiny web app, with Python helpers, that gives a new client Internet access in one form: it reads the client from the Xgest ERP, creates the PPPoE and HotSpot users on two RADIUS servers, pushes the router's config over TFTP (DHCP option 66) or GenieACS (TR-069), and opens the Web Help Desk ticket.

[Project presented as a Final Project at University in 2015](http://repositorio.upct.es/xmlui/handle/10317/5245). The FAQ ([`FAQ/FAQ.Rmd`](FAQ/FAQ.Rmd)) is in Spanish.

## Quick start

You need R with `shiny`, `shinyjs`, `RMySQL`, `digest` and `stringr`, plus MySQL access to Xgest and the RADIUS servers and a GenieACS instance. The code expects the checkout at `/home/tecnico/WebApp` (`server.R` line 22 sets it as the working directory); clone it there or edit that line. Set the hosts and passwords in `server.R` (lines 28, 290, 336, 356, 454, 577, 629), then:

```bash
Rscript -e 'shiny::runApp(".", port = 8081, host = "127.0.0.1")'
```

Open http://127.0.0.1:8081. Ticket creation shells out to `tickets.py` (Python 2, Selenium, PhantomJS).

## Related projects

- [genieacs-container](https://github.com/GeiserX/genieacs-container): Helm chart and container for GenieACS TR-069
- [services-isp](https://github.com/GeiserX/services-isp): automates common ISP operational tasks
- [statix](https://github.com/GeiserX/statix): ISP network statistics dashboard
- [ScriptPoblar](https://github.com/GeiserX/ScriptPoblar): adopts a whole network of devices into CRM Control in parallel

## License

[GPL-3.0-or-later](LICENSE)
