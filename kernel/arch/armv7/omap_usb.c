

/*************************************************************************
 * 
 * USB host intialization.  This will move out of the CPU driver into
 * user space in due course.
 *
 */

#include <dev/omap/omap44xx_hsusbhost_dev.h>
#include <dev/omap/omap44xx_usbtllhs_config_dev.h>
#include <dev/omap/omap44xx_scrm_dev.h>
#include <dev/omap/omap44xx_sysctrl_padconf_wkup_dev.h>
#include <dev/omap/omap44xx_sysctrl_padconf_core_dev.h>
#include <dev/omap/omap44xx_ehci_dev.h>
#include <dev/omap/omap44xx_ckgen_prm_dev.h>
#include <dev/omap/omap44xx_l4per_cm2_dev.h>
#include <dev/omap/omap44xx_l3init_cm2_dev.h>

/* 
 * Forward declarations
 */
static void hsusb_init(void);
static void usb_power_on(void);
static void prcm_init(void);
static void set_muxconf_regs(void);

#define KERNEL_DEBUG_USB 0

/* mackerel base addresses for USB initialization */
#define OMAP44XX_USBTLLHS_CONFIG 0x4A062000
#define OMAP44XX_HSUSBHOST  0x4A064000
#define OMAP44XX_SCRM 0x4A30A000
#define OMAP44XX_SYSCTRL_PADCONF_WKUP 0x4A31E000
#define OMAP44XX_SYSCTRL_PADCONF_CORE 0x4A100000
#define OMAP44XX_EHCI 0x4A064C00
#define OMAP44XX_CKGEN_PRM 0x4A306100
#define OMAP44XX_L4PER_CM2 0x4A009400
#define OMAP44XX_L3INIT_CM2 0x4A009300

/* mackerel bases for USB initialization */
static omap44xx_hsusbhost_t hsusbhost_base;
static omap44xx_usbtllhs_config_t usbtllhs_config_base;
static omap44xx_scrm_t srcm_base;
static omap44xx_sysctrl_padconf_wkup_t sysctrl_padconf_wkup_base;
static omap44xx_sysctrl_padconf_core_t sysctrl_padconf_core_base;
static omap44xx_gpio_t gpio_1_base;
static omap44xx_gpio_t gpio_2_base;
static omap44xx_ehci_t ehci_base;
static omap44xx_ckgen_prm_t ckgen_base;
static omap44xx_l4per_cm2_t l4per_base;
static omap44xx_l3init_cm2_t l3init_base;

// GPIO numbers for enabling the USB hub on the pandaboard
#define HSUSB_HUB_POWER 1
#define HSUSB_HUB_RESET 30


/*
 * pandaboard related USB setup
 */
static void usb_host_init(void)
{
    printf("-------------------------\nUSB Host initialization\n");
    omap44xx_hsusbhost_initialize(&hsusbhost_base,
				  (mackerel_addr_t) OMAP44XX_HSUSBHOST);
    omap44xx_usbtllhs_config_initialize(&usbtllhs_config_base,
					(mackerel_addr_t) OMAP44XX_USBTLLHS_CONFIG);
    omap44xx_scrm_initialize(&srcm_base, (mackerel_addr_t) OMAP44XX_SCRM);
    omap44xx_sysctrl_padconf_wkup_initialize(&sysctrl_padconf_wkup_base,
					     (mackerel_addr_t) OMAP44XX_SYSCTRL_PADCONF_WKUP);
    omap44xx_sysctrl_padconf_core_initialize(&sysctrl_padconf_core_base,
					     (mackerel_addr_t) OMAP44XX_SYSCTRL_PADCONF_CORE);
    omap44xx_gpio_initialize(&gpio_1_base,
			     (mackerel_addr_t) OMAP44XX_MAP_L4_WKUP_GPIO1);
    omap44xx_gpio_initialize(&gpio_2_base,
			     (mackerel_addr_t) OMAP44XX_MAP_L4_PER_GPIO2);
    omap44xx_ehci_initialize(&ehci_base, (mackerel_addr_t) OMAP44XX_EHCI);
    
    omap44xx_ckgen_prm_initialize(&ckgen_base,
				  (mackerel_addr_t) OMAP44XX_CKGEN_PRM);
    omap44xx_l4per_cm2_initialize(&l4per_base,
				  (mackerel_addr_t) OMAP44XX_L4PER_CM2);
    omap44xx_l3init_cm2_initialize(&l3init_base,
				   (mackerel_addr_t) OMAP44XX_L3INIT_CM2);
    prcm_init();
    set_muxconf_regs();
    usb_power_on();
    printf("-------------------------\n");
}

/*
 * initialize the USB functionality of the pandaboard
 */
static void hsusb_init(void)
{

    printf("  > hsusb_init()...\n");

    /*
     * Global Initialization of the OMAPP44xx USB Sub System
     */
    printf("  >  > USB TTL reset...");

    /*
     * Reset USBTTL
     * USBTLL_SYSCONFIG = 0x2
     */
    omap44xx_usbtllhs_config_usbtll_sysconfig_softreset_wrf(
            &usbtllhs_config_base, 0x1);

    /*
     * wait till reset is done
     */
    while (!omap44xx_usbtllhs_config_usbtll_sysstatus_resetdone_rdf(
            &usbtllhs_config_base))
        ;

    /*
     * USBTLL_SYSCONFIG
     *  - Setting ENAWAKEUP
     *  - Setting SIDLEMODE
     *  - Setting CLOCKACTIVITY
     */
    omap44xx_usbtllhs_config_usbtll_sysconfig_t sysconf = 0x0;
    sysconf = omap44xx_usbtllhs_config_usbtll_sysconfig_clockactivity_insert(
            sysconf, 0x1);
    sysconf = omap44xx_usbtllhs_config_usbtll_sysconfig_enawakeup_insert(
            sysconf, 0x1);
    sysconf = omap44xx_usbtllhs_config_usbtll_sysconfig_sidlemode_insert(
            sysconf, 0x1);
    omap44xx_usbtllhs_config_usbtll_sysconfig_wr(&usbtllhs_config_base,
            sysconf);

    printf("OK\n");

    /*
     * USBTLL_IRQENABLE:
     *  - all interrupts
     */
    omap44xx_usbtllhs_config_usbtll_irqenable_t irqena = omap44xx_usbtllhs_config_usbtll_irqenable_default;
    irqena = omap44xx_usbtllhs_config_usbtll_irqenable_fclk_start_en_insert(
            irqena, 0x1);
    irqena = omap44xx_usbtllhs_config_usbtll_irqenable_fclk_end_en_insert(
            irqena, 0x1);
    irqena = omap44xx_usbtllhs_config_usbtll_irqenable_access_error_en_insert(
            irqena, 0x1);
    omap44xx_usbtllhs_config_usbtll_irqenable_wr(&usbtllhs_config_base, irqena);

    printf("  >  > USB host controller reset...");

    /*
     * per form a reset on the USB host controller module
     * this resets both EHCI and OCHI controllers
     *
     * UHH_SYSCONFIG = 0x1
     */
    omap44xx_hsusbhost_uhh_sysconfig_softreset_wrf(&hsusbhost_base, 0x1);

    /*
     * wait till reset is done
     * UHH_SYSSTATUS = 0x6
     */
    omap44xx_hsusbhost_uhh_sysstatus_t uhh_sysstat;
    uint8_t ehci_done;
    uint8_t ohci_done;
    do {
        uhh_sysstat = omap44xx_hsusbhost_uhh_sysstatus_rd(&hsusbhost_base);
        ehci_done = omap44xx_hsusbhost_uhh_sysstatus_ehci_resetdone_extract(
                uhh_sysstat);
        ohci_done = omap44xx_hsusbhost_uhh_sysstatus_ohci_resetdone_extract(
                uhh_sysstat);
    } while (!(ehci_done & ohci_done));

    /* enable some USB host features
     * UHH_SYSCONFIG
     *  - STANDBYMODE
     *  - IDLEMODE
     */
    omap44xx_hsusbhost_uhh_sysconfig_standbymode_wrf(&hsusbhost_base, 0x1);
    omap44xx_hsusbhost_uhh_sysconfig_idlemode_wrf(&hsusbhost_base, 0x1);

    printf("OK\n");

    printf("  >  > Setting USB host configuration values...");

    /*
     * setting the host configuration to external PHY and enable
     * the burst types, app start clk
     *
     * UHH_HOSTCONFIG
     *  - APP_START_CLK
     *  - ENAINCR_x
     */
    // *((volatile uint32_t*) (0x4A064040)) =
    //       (uint32_t) ((0x7 << 2) | (0x1 << 31));
    omap44xx_hsusbhost_uhh_hostconfig_t hcfg = omap44xx_hsusbhost_uhh_hostconfig_default;
    hcfg = omap44xx_hsusbhost_uhh_hostconfig_app_start_clk_insert(hcfg, 0x1);
    hcfg = omap44xx_hsusbhost_uhh_hostconfig_ena_incr4_insert(hcfg, 0x1);
    hcfg = omap44xx_hsusbhost_uhh_hostconfig_ena_incr8_insert(hcfg, 0x1);
    hcfg = omap44xx_hsusbhost_uhh_hostconfig_ena_incr16_insert(hcfg, 0x1);
    omap44xx_hsusbhost_uhh_hostconfig_wr(&hsusbhost_base, hcfg);

    printf("OK\n");

    printf("  > done.\n");
}

/*
 * Initialize the high speed usb hub on the pandaboard
 */
static void usb_power_on(void)
{
    printf("usb_power_on()... \n");

    printf("  > forward the AUXCLK3 to GPIO_WK31\n");
    /*
     * the USB hub needs the FREF_CLK3_OUT to be 19.2 MHz and that this
     * clock goes to the GPIO_WK31 out.
     * Assume that the sys clock is 38.4 MHz so we use a divider of 2
     *
     * Bit  8: is the enable bit
     * Bit 16: is the divider bit (here for two)
     */

    omap44xx_scrm_auxclk3_t auxclk3 = omap44xx_scrm_auxclk3_default;
    auxclk3 = omap44xx_scrm_auxclk3_enable_insert(auxclk3,
            omap44xx_scrm_ENABLE_EXT_1);
    auxclk3 = omap44xx_scrm_auxclk3_clkdiv_insert(auxclk3,
            omap44xx_scrm_MODE_1);
    omap44xx_scrm_auxclk3_wr(&srcm_base, auxclk3);

    /*
     * Forward the clock to the GPIO_WK31 pin
     *  - muxmode = fref_clk3_out (0x0)
     *  - no pullup/down (0x0)
     *  - no input buffer (0x0)
     *  - no wake up (0x0)
     */
    omap44xx_sysctrl_padconf_wkup_control_wkup_pad0_fref_clk3_out_pad1_fref_clk4_req_t clk3_out;
    clk3_out = omap44xx_sysctrl_padconf_wkup_control_wkup_pad0_fref_clk3_out_pad1_fref_clk4_req_rd(
            &sysctrl_padconf_wkup_base);
    clk3_out = omap44xx_sysctrl_padconf_wkup_control_wkup_pad0_fref_clk3_out_pad1_fref_clk4_req_fref_clk3_out_muxmode_insert(
            clk3_out, 0x0);
    clk3_out = omap44xx_sysctrl_padconf_wkup_control_wkup_pad0_fref_clk3_out_pad1_fref_clk4_req_fref_clk3_out_pulludenable_insert(
            clk3_out, 0x0);
    clk3_out = omap44xx_sysctrl_padconf_wkup_control_wkup_pad0_fref_clk3_out_pad1_fref_clk4_req_fref_clk3_out_pulltypeselect_insert(
            clk3_out, 0x0);
    clk3_out = omap44xx_sysctrl_padconf_wkup_control_wkup_pad0_fref_clk3_out_pad1_fref_clk4_req_fref_clk3_out_inputenable_insert(
            clk3_out, 0x0);
    clk3_out = omap44xx_sysctrl_padconf_wkup_control_wkup_pad0_fref_clk3_out_pad1_fref_clk4_req_fref_clk3_out_wakeupenable_insert(
            clk3_out, 0x0);
    clk3_out = omap44xx_sysctrl_padconf_wkup_control_wkup_pad0_fref_clk3_out_pad1_fref_clk4_req_fref_clk3_out_wakeupevent_insert(
            clk3_out, 0x0);
    omap44xx_sysctrl_padconf_wkup_control_wkup_pad0_fref_clk3_out_pad1_fref_clk4_req_wr(
            &sysctrl_padconf_wkup_base, clk3_out);

    printf("  > reset external USB hub and PHY\n");

    /*
     * Perform a reset on the USB hub i.e. drive the GPIO_1 pin to low
     * and enable the dataout for the this pin in GPIO
     */

    uint32_t gpoi_1_oe = omap44xx_gpio_oe_rd(&gpio_1_base)
            & (~(1UL << HSUSB_HUB_POWER));
    omap44xx_gpio_oe_wr(&gpio_1_base, gpoi_1_oe);

    omap44xx_gpio_cleardataout_wr(&gpio_1_base, (1UL << HSUSB_HUB_POWER));

    /*
     * forward the data outline to the USB hub by muxing the
     * CONTROL_CORE_PAD0_KPD_COL1_PAD1_KPD_COL2 into mode 3 (gpio_1)
     */

    omap44xx_sysctrl_padconf_core_control_core_pad0_kpd_col1_pad1_kpd_col2_t gpio1_mux;
    gpio1_mux = omap44xx_sysctrl_padconf_core_control_core_pad0_kpd_col1_pad1_kpd_col2_rd(
            &sysctrl_padconf_core_base) & 0x0000FFFF;
    gpio1_mux = omap44xx_sysctrl_padconf_core_control_core_pad0_kpd_col1_pad1_kpd_col2_kpd_col2_muxmode_insert(
            gpio1_mux, 0x3);
    omap44xx_sysctrl_padconf_core_control_core_pad0_kpd_col1_pad1_kpd_col2_wr(
            &sysctrl_padconf_core_base, gpio1_mux);

    /*
     * Perform a reset on the USB phy i.e. drive GPIO_62 to low
     *
     * HSUSB_HUB_RESET: 0 = Hub & Phy held in reset     1 = Normal operation.
     */

    uint32_t gpoi_2_oe = omap44xx_gpio_oe_rd(&gpio_2_base)
            & (~(1UL << HSUSB_HUB_RESET));
    omap44xx_gpio_oe_wr(&gpio_2_base, gpoi_2_oe);

    omap44xx_gpio_cleardataout_wr(&gpio_2_base, (1UL << HSUSB_HUB_RESET));

    /*
     * forward the data on gpio_62 pin to the output by muxing
     *  CONTROL_CORE_PAD0_GPMC_WAIT1_PAD1_GPMC_WAIT2 to mode 0x3
     */

    omap44xx_sysctrl_padconf_core_control_core_pad0_gpmc_wait1_pad1_gpmc_wait2_t gpio62_mux;
    gpio62_mux = (omap44xx_sysctrl_padconf_core_control_core_pad0_gpmc_wait1_pad1_gpmc_wait2_rd(
            &sysctrl_padconf_core_base) & 0xFFFF0000);
    gpio62_mux = omap44xx_sysctrl_padconf_core_control_core_pad0_gpmc_wait1_pad1_gpmc_wait2_gpmc_wait1_muxmode_insert(
            gpio62_mux, 0x3);
    omap44xx_sysctrl_padconf_core_control_core_pad0_gpmc_wait1_pad1_gpmc_wait2_wr(
            &sysctrl_padconf_core_base, gpio62_mux);
    omap44xx_sysctrl_padconf_core_control_core_pad0_gpmc_wait1_pad1_gpmc_wait2_wr(
            &sysctrl_padconf_core_base, gpio62_mux);

    /* delay to give the hardware time to reset TODO: propper delay*/
    for (int j = 0; j < 2000; j++) {
        printf("%c", 0x20);
        printf("%c", 0x08);
    }

    hsusb_init();

    printf("  > enable the external USB hub and PHY\n");

    /* power on the USB subsystem */
    omap44xx_gpio_setdataout_wr(&gpio_1_base, (1UL << HSUSB_HUB_POWER));

    /* enable the USB HUB */
    omap44xx_gpio_setdataout_wr(&gpio_2_base, (1UL << HSUSB_HUB_RESET));

    for (int j = 0; j < 2000; j++) {
        printf("%c", 0x20);
        printf("%c", 0x08);
    }

    printf("  > performing softreset on the USB PHY\n");

    omap44xx_ehci_insnreg05_ulpi_t ulpi = omap44xx_ehci_insnreg05_ulpi_default;
    ulpi = omap44xx_ehci_insnreg05_ulpi_control_insert(ulpi,
            omap44xx_ehci_CONTROL_1);
    ulpi = omap44xx_ehci_insnreg05_ulpi_portsel_insert(ulpi,
            omap44xx_ehci_PORTSEL_1);
    ulpi = omap44xx_ehci_insnreg05_ulpi_opsel_insert(ulpi,
            omap44xx_ehci_OPSEL_2);
    ulpi = omap44xx_ehci_insnreg05_ulpi_regadd_insert(ulpi, 0x5);  //ctrl reg
    ulpi = omap44xx_ehci_insnreg05_ulpi_rdwrdata_insert(ulpi, (0x1 << 5));

    omap44xx_ehci_insnreg05_ulpi_wr(&ehci_base, ulpi);

    while (omap44xx_ehci_insnreg05_ulpi_control_rdf(&ehci_base)) {
        printf("%c", 0xE);
    }

    try_again:
    /* wait till reset is done */
    ulpi = omap44xx_ehci_insnreg05_ulpi_opsel_insert(ulpi,
            omap44xx_ehci_OPSEL_3);
    ulpi = omap44xx_ehci_insnreg05_ulpi_rdwrdata_insert(ulpi, 0x0);
    omap44xx_ehci_insnreg05_ulpi_wr(&ehci_base, ulpi);

    while (omap44xx_ehci_insnreg05_ulpi_control_rdf(&ehci_base)) {
        printf("%c", 0xE);
    }
    if (omap44xx_ehci_insnreg05_ulpi_rdwrdata_rdf(&ehci_base) & (0x1 << 5)) {
        goto try_again;
    }

    /* read the debug register */
    ulpi = omap44xx_ehci_insnreg05_ulpi_regadd_insert(ulpi, 0x15);
    omap44xx_ehci_insnreg05_ulpi_wr(&ehci_base, ulpi);

    while (omap44xx_ehci_insnreg05_ulpi_control_rdf(&ehci_base)) {
        printf("%c", 0xE);
    }

    uint8_t line_state = omap44xx_ehci_insnreg05_ulpi_rdwrdata_rdf(&ehci_base) & 0x1;
    printf("  > ULPI line state = %s\n",
            line_state ? "Connected" : "Disconnected");
    assert(line_state);

    printf("done.\n");
}

static void prcm_init(void)
{
    printf("prcm_init()...\n");

    printf("  > CM_SYS_CLKSEL=38.4MHz \n");
    /*
     * Set the system clock to 38.4 MHz
     * CM_SYS_CLKSEL = 0x7
     */

    omap44xx_ckgen_prm_cm_sys_clksel_wr(&ckgen_base,
            omap44xx_ckgen_prm_SYS_CLKSEL_7);

    if (!omap44xx_ckgen_prm_cm_sys_clksel_rd(&ckgen_base)) {
        printf("WARNING: Could not set SYS_CLK\n");
        return;
    }

    /* ALTCLKSRC in SRCM*/
    omap44xx_scrm_altclksrc_t altclk = omap44xx_scrm_altclksrc_default;
    altclk = omap44xx_scrm_altclksrc_mode_insert(altclk, omap44xx_scrm_MODE_1);
    altclk = omap44xx_scrm_altclksrc_enable_int_insert(altclk, 0x1);
    altclk = omap44xx_scrm_altclksrc_enable_ext_insert(altclk, 0x1);
    omap44xx_scrm_altclksrc_wr(&srcm_base, altclk);

    printf("  > Enabling L4PER interconnect clock\n");
    /* CM_L4PER_CLKSTCTRL */
    omap44xx_l4per_cm2_cm_l4per_clkstctrl_clktrctrl_wrf(&l4per_base, 0x2);

    printf("  > Enabling GPIOi clocks\n");
    /* CM_L4PER_GPIO2_CLKCTRL */
    omap44xx_l4per_cm2_cm_l4per_gpio2_clkctrl_modulemode_wrf(&l4per_base, 0x1);
    /* CM_L4PER_GPIO3_CLKCTRL */
    omap44xx_l4per_cm2_cm_l4per_gpio3_clkctrl_modulemode_wrf(&l4per_base, 0x1);
    /* CM_L4PER_GPIO4_CLKCTRL */
    omap44xx_l4per_cm2_cm_l4per_gpio4_clkctrl_modulemode_wrf(&l4per_base, 0x1);
    /* CM_L4PER_GPIO5_CLKCTRL */
    omap44xx_l4per_cm2_cm_l4per_gpio5_clkctrl_modulemode_wrf(&l4per_base, 0x1);
    /* CM_L4PER_GPIO6_CLKCTRL */
    omap44xx_l4per_cm2_cm_l4per_gpio6_clkctrl_modulemode_wrf(&l4per_base, 0x1);
    /* CM_L4PER_HDQ1W_CLKCTRL */
    omap44xx_l4per_cm2_cm_l4per_hdq1w_clkctrl_modulemode_wrf(&l4per_base, 0x2);

    printf("  > Enabling L3INIT USB clocks\n");
    /* CM_L3INIT_HSI_CLKCTRL */
    omap44xx_l3init_cm2_cm_l3init_hsi_clkctrl_modulemode_wrf(&l3init_base, 0x1);

    /* CM_L3INIT_HSUSBHOST_CLKCTRL */
    omap44xx_l3init_cm2_cm_l3init_hsusbhost_clkctrl_t hsusb_cm = 0x0;
    hsusb_cm = omap44xx_l3init_cm2_cm_l3init_hsusbhost_clkctrl_clksel_utmi_p1_insert(
            hsusb_cm, 0x3);
    hsusb_cm = omap44xx_l3init_cm2_cm_l3init_hsusbhost_clkctrl_modulemode_insert(
            hsusb_cm, 0x2);
    hsusb_cm |= 0xFF00;  // all clocks
    omap44xx_l3init_cm2_cm_l3init_hsusbhost_clkctrl_wr(&l3init_base, hsusb_cm);

    /* CM_L3INIT_HSUSBOTG_CLKCTRL */
    omap44xx_l3init_cm2_cm_l3init_hsusbotg_clkctrl_modulemode_wrf(&l3init_base,
            0x1);

    /* CM_L3INIT_HSUSBTLL_CLKCTRL */
    omap44xx_l3init_cm2_cm_l3init_hsusbtll_clkctrl_t usbtll_cm = 0x0;
    usbtll_cm = omap44xx_l3init_cm2_cm_l3init_hsusbtll_clkctrl_modulemode_insert(
            usbtll_cm, 0x1);
    usbtll_cm = omap44xx_l3init_cm2_cm_l3init_hsusbtll_clkctrl_optfclken_usb_ch0_clk_insert(
            usbtll_cm, 0x1);
    usbtll_cm = omap44xx_l3init_cm2_cm_l3init_hsusbtll_clkctrl_optfclken_usb_ch1_clk_insert(
            usbtll_cm, 0x1);
    omap44xx_l3init_cm2_cm_l3init_hsusbtll_clkctrl_wr(&l3init_base, usbtll_cm);

    /* CM_L3INIT_FSUSB_CLKCTRL */
    omap44xx_l3init_cm2_cm_l3init_fsusb_clkctrl_modulemode_wrf(&l3init_base,
            0x2);
    /* CM_L3INIT_USBPHY_CLKCTRL */
    omap44xx_l3init_cm2_cm_l3init_usbphy_clkctrl_wr(&l3init_base, 0x301);

    printf("done.\n");
}

static void set_muxconf_regs(void)
{
    printf("set_muxconf_regs()...");

    /* CONTROL_PADCONF_CORE_SYSCONFIG */
    omap44xx_sysctrl_padconf_core_control_padconf_core_sysconfig_ip_sysconfig_idlemode_wrf(
            &sysctrl_padconf_core_base, 0x1);

    /* CONTROL_PADCONF_WKUP_SYSCONFIG */
    omap44xx_sysctrl_padconf_wkup_control_padconf_wkup_sysconfig_ip_sysconfig_idlemode_wrf(
            &sysctrl_padconf_wkup_base, 0x1);

    /* USBB1_CLK */
    omap44xx_sysctrl_padconf_core_control_core_pad0_cam_globalreset_pad1_usbb1_ulpitll_clk_t ulpitll;
    ulpitll = omap44xx_sysctrl_padconf_core_control_core_pad0_cam_globalreset_pad1_usbb1_ulpitll_clk_rd(
            &sysctrl_padconf_core_base) & 0x0000FFFF;
    ulpitll = omap44xx_sysctrl_padconf_core_control_core_pad0_cam_globalreset_pad1_usbb1_ulpitll_clk_usbb1_ulpitll_clk_inputenable_insert(
            ulpitll, 0x1);
    ulpitll = omap44xx_sysctrl_padconf_core_control_core_pad0_cam_globalreset_pad1_usbb1_ulpitll_clk_usbb1_ulpitll_clk_pulludenable_insert(
            ulpitll, 0x1);
    ulpitll = omap44xx_sysctrl_padconf_core_control_core_pad0_cam_globalreset_pad1_usbb1_ulpitll_clk_usbb1_ulpitll_clk_muxmode_insert(
            ulpitll, 0x4);
    omap44xx_sysctrl_padconf_core_control_core_pad0_cam_globalreset_pad1_usbb1_ulpitll_clk_wr(
            &sysctrl_padconf_core_base, ulpitll);

    /* USBB1_STP / USBB1_DIR */
    omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_stp_pad1_usbb1_ulpitll_dir_t usb_dir = 0x0;
    usb_dir = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_stp_pad1_usbb1_ulpitll_dir_usbb1_ulpitll_stp_muxmode_insert(
            usb_dir, 0x4);
    usb_dir = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_stp_pad1_usbb1_ulpitll_dir_usbb1_ulpitll_dir_muxmode_insert(
            usb_dir, 0x4);
    usb_dir = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_stp_pad1_usbb1_ulpitll_dir_usbb1_ulpitll_dir_inputenable_insert(
            usb_dir, 0x1);
    usb_dir = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_stp_pad1_usbb1_ulpitll_dir_usbb1_ulpitll_dir_pulludenable_insert(
            usb_dir, 0x1);
    omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_stp_pad1_usbb1_ulpitll_dir_wr(
            &sysctrl_padconf_core_base, usb_dir);

    /* this values are used for all the 8 data lines */
    uint32_t usb_dat = 0x0;
    usb_dat = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_nxt_pad1_usbb1_ulpitll_dat0_usbb1_ulpitll_dat0_muxmode_insert(
            usb_dat, 0x4);
    usb_dat = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_nxt_pad1_usbb1_ulpitll_dat0_usbb1_ulpitll_nxt_muxmode_insert(
            usb_dat, 0x4);
    usb_dat = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_nxt_pad1_usbb1_ulpitll_dat0_usbb1_ulpitll_dat0_inputenable_insert(
            usb_dat, 0x1);
    usb_dat = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_nxt_pad1_usbb1_ulpitll_dat0_usbb1_ulpitll_nxt_inputenable_insert(
            usb_dat, 0x1);
    usb_dat = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_nxt_pad1_usbb1_ulpitll_dat0_usbb1_ulpitll_dat0_pulludenable_insert(
            usb_dat, 0x1);
    usb_dat = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_nxt_pad1_usbb1_ulpitll_dat0_usbb1_ulpitll_nxt_pulludenable_insert(
            usb_dat, 0x1);

    /* USBB1_DAT0 / USBB1_NXT */
    omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_nxt_pad1_usbb1_ulpitll_dat0_wr(
            &sysctrl_padconf_core_base, usb_dat);

    /* USBB1_DAT1 / USBB1_DAT2 */
    omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_dat1_pad1_usbb1_ulpitll_dat2_wr(
            &sysctrl_padconf_core_base, usb_dat);

    /* USBB1_DAT3 / USBB1_DAT4 */
    omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_dat3_pad1_usbb1_ulpitll_dat4_wr(
            &sysctrl_padconf_core_base, usb_dat);

    /* USBB1_DAT5 / USBB1_DAT6 */
    omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_dat5_pad1_usbb1_ulpitll_dat6_wr(
            &sysctrl_padconf_core_base, usb_dat);

    /* USBB1_DAT7 */
    omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_dat7_pad1_usbb1_hsic_data_t usb_dat7;
    usb_dat7 = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_dat7_pad1_usbb1_hsic_data_rd(
            &sysctrl_padconf_core_base) & 0xFFFF0000;
    usb_dat7 = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_dat7_pad1_usbb1_hsic_data_usbb1_ulpitll_dat7_muxmode_insert(
            usb_dat7, 0x4);
    usb_dat7 = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_dat7_pad1_usbb1_hsic_data_usbb1_ulpitll_dat7_pulludenable_insert(
            usb_dat7, 0x1);
    usb_dat7 = omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_dat7_pad1_usbb1_hsic_data_usbb1_ulpitll_dat7_inputenable_insert(
            usb_dat7, 0x1);
    omap44xx_sysctrl_padconf_core_control_core_pad0_usbb1_ulpitll_dat7_pad1_usbb1_hsic_data_wr(
            &sysctrl_padconf_core_base, usb_dat7);

    printf("done\n");
}
