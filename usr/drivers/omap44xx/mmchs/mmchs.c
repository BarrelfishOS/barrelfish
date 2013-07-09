 /*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/inthandler.h>

#include <driverkit/driverkit.h>

#include "omap44xx_mmchs.h"
#include "omap44xx_cm2.h"
#include "omap44xx_ctrlmod.h"
#include "ti_twl6030.h"

#include <dev/omap/omap44xx_mmchs_dev.h>
#include <dev/sdhc_dev.h>

#include <gic.h>

static sdhc_t sdhc;
static omap44xx_mmchs_t mmchs;

// XXX Temporarily disable annoying warnings about unused functions!!
#pragma GCC diagnostic ignored "-Wunused-function" 
#pragma GCC diagnostic ignored "-Wsuggest-attribute=noreturn"

#define PBS (64*1024)
static char PRBUF[PBS];
#define PRBUFL PRBUF, (PBS-1)

/*
 * \brief Set divider for frequency configuration
 */
static void mmchs_set_frequency(int freq_div)
{
    int base_clock;
    base_clock = cm2_get_hsmmc1_base_clock();

    printf("mmchs: setting clock frequency to %d Hz\n",
           (base_clock/freq_div));
    sdhc_sysctl_clkd_wrf(&sdhc, freq_div);

}

/*
 * \brief Do a software reset of the MMC module
 *
 * Reset is only done if resetdone flag is not 1 already .. 
 *
 * Otherwise there is a risk that we cannot detect when the second reset
 * is acutally completed (since the flag is 1 alread) which might make us
 * loose some of the future actions on the MMC since it is not ready.
 *
 * After powering up the PandaBoard, RESETDONE usually is 1 already 
 *
 * Figure 24-36 
 */
static void mmchs_do_sw_reset(void)
{
    printf("mmchs: do_sw_reset()\n");
    omap44xx_mmchs_SYSSTATUS_pr(PRBUFL,&mmchs);
    printf("mmchs:  %s", PRBUF);
    omap44xx_mmchs_HL_SYSCONFIG_SOFTRESET_wrf(&mmchs, 0x1);

    volatile uint32_t l;
    for(l = 0; omap44xx_mmchs_HL_SYSCONFIG_SOFTRESET_rdf(&mmchs) == 1; l++);
    printf("mmchs:  waited %d loops for HL_SYSCONFIG_SOFTRESET\n", l);
    for(l = 0; omap44xx_mmchs_SYSCONFIG_SOFTRESET_rdf(&mmchs) == 1; l++);
    printf("mmchs:  waited %d loops for SYSCONFIG_SOFTRESET\n", l);
    for(l = 0; omap44xx_mmchs_SYSSTATUS_RESETDONE_rdf(&mmchs) == 0; l++);
    printf("mmchs:  waited %d loops for SYSSTATUS_RESETDONE\n", l);
}


/*
 * \brief Reset command and data state machine
 *
 * See BSD code ti_mmchs.c line 1357
 */
static void mmchs_do_state_machine_reset(void)
{
    volatile uint32_t l;
    printf("mmchs: do_state_machine_reset()\n");

    sdhc_sysctl_sra_wrf(&sdhc, 0x1);
    for(l = 0; sdhc_sysctl_sra_rdf(&sdhc) != 0; l++);
    printf("mmchs:  waited %d loops for sysctl_sra\n", l);
}


/*
 * \brief Print MMCHS registers
 */
static void print_mmchs(void)
{
    omap44xx_mmchs_pr(PRBUFL, &mmchs);
    printf("%s\n", PRBUF);
    sdhc_pr(PRBUFL, &sdhc);
    printf("%s\n", PRBUF);
}

/*
 * \brief Print MMC error state
 */
static void print_mmchs_state(void)
{
    sdhc_stat_pr(PRBUFL, &sdhc);
    printf("%s\n", PRBUF);

    sdhc_ps_pr(PRBUFL, &sdhc);
    printf("%s\n", PRBUF);

}

volatile uint32_t dummy = 0;

/*
 * \brief Sleep for @msec miliseconds
 *
 * XXX I am sure there is a better way to do this ..
 */
static void mmchs_wait_msec(long msec)
{
    int i=0, sum = 0; 
    long end = (1200000*msec/8);

    // Cannot use volatile variables in loop
    while (++i<end)  {
        sum+=i+end;
    }

    dummy += sum;
}

/*
 * \brief Do CMD line reset
 *
 * TRM 24.5.1.2.1.2, page 5120
 */
static void mmchs_cmd_line_reset(void)
{
    sdhc_sysctl_src_wrf(&sdhc, 0x1);
    while (sdhc_sysctl_src_rdf(&sdhc)!=0x1)
        ;
    while (sdhc_sysctl_src_rdf(&sdhc)!=0x0)
        ;
}

/*
 * \brief Initialize and identify
 *
 * This is according to SD card spec v3, chp 3.6
 */
int mmchs_init_and_ident_card(void)
{
    // Reset card
    // XXX Keep pin 1 high during execution??
    printf("identify: cmd0\n");
    mmchs_send_cmd(0, 0);

    mmchs_wait_msec(1000);

    // Free BSD: mmc_send_if_conf [mmc.c]
    // Sets vhs, and uses 0xAA as pattern
    uint32_t cmd8arg = 0;
    sdhc_cmd8_t cp= (sdhc_cmd8_t)&cmd8arg;
    // 2.7-3.6V (seems to  be the only value allowed .. )
    sdhc_cmd8_vhs_insert(cp, 0x1);
    // Recommended pattern is: 0x1010101010
    sdhc_cmd8_pattern_insert(cp, 0xaa);
    printf("identify: cmd8\n");
    mmchs_send_cmd(8, cmd8arg);

    // Wait for result
    return mmchs_finalize_cmd();
}

/*
 * \brief TBD
 *
 * see TRM Figure 24-38 left-handed
 * and BSD code in ti_mmchs_send_init_stream in ti_mmchs.c
 *
 * Free BSD has a slightly different approach compared to the
 * TRM. They issue CMD0 commands and wait for the STAT_CC bit to be
 * set, whereas the TRM just waits for 1 ms.
 *
 * Precondition: Module initiliaztion
 */
static void mmchs_init_stream(void)
{
    printf("mmchs: initializing stream .. \n");

    // Make sure that the command line is not busy at this point 
    assert (sdhc_ps_cmdi_rdf(&sdhc)==0x0);

    // Save original IE and ISE registers
    sdhc_ir_t ie = sdhc_ie_rd(&sdhc);
    sdhc_ir_t ise = sdhc_ise_rd(&sdhc);

    // Enable interrupts, so that CC will be set
    // These are SD card interrupts.
    // If we don't do that, STAT_CC will not be set
    // BSD line 894
    // XXX Don't use constant here .. 
    sdhc_ie_wr(&sdhc, 0x307F0033);

    // Disable genernation of status events
    sdhc_ise_wr(&sdhc, 0x0);

    // Initialize stream
    omap44xx_mmchs_CON_INIT_wrf(&mmchs, 0x1); 
    // Writing a dummy command
    // CMD0 seems to be a reset command ..
    sdhc_ctm_rawwr(&sdhc, 0x0);
    // Wait for completion of command
    while (sdhc_stat_cc_rdf(&sdhc)!=0x1) ;
    mmchs_wait_msec(1);
    // Clear status field, so that we see when CC (command complete) sticks
    sdhc_stat_cc_wrf(&sdhc, 0x1);

    // Need to do the whole procedure twice, since we cannot
    // make our clock slow enough to not have more  than 80 cycles 
    // in one msec .. 

    // End initialization sequence ...
    omap44xx_mmchs_CON_INIT_wrf(&mmchs, 0x0);

    printf("mmchs: stream init done (how do we know it works?)\n");

    // Reset the interrupt configuration
    sdhc_ie_wr(&sdhc, ie);
    sdhc_ise_wr(&sdhc, ise);

    // Unset STAT 
    sdhc_stat_rawwr(&sdhc, ~0x0);
    /* sdhc_stat_rd(&sdhc); // << why are we reading this (from BSD) */

    // Change clock frequency to fit protocol?
    // TRM 24.5.1.2.1.7.2
    /* mmchs_change_clock_frequency(); */

    mmchs_cmd_line_reset();

    // Make sure nothing went wrong in init
    /* assert(sdhc_stat_bada_rdf(&sdhc)==0x0); */
    /* assert(sdhc_stat_erri_rdf(&sdhc)==0x0); */
}

/* 
 * \brief Identify card
 *
 * Does currently not work! Report all card types being present, 
 * no matter what and if any card is actually inserted!
 *
 * This is according to TI TRM
 */
void mmchs_identify_card(void)
{
    // << OMAP TRM method does not work!

    // send a CMD0 command
    // TRM 24.5.1.2.1.7.1
    mmchs_send_cmd(CMD0, 0);

    // send a CMD5 command
    // XXX don't use the wrapper function    mmchs_send_cmd(CMD5);

    // Wait for CMD line to become available
    while (sdhc_ps_cmdi_rdf(&sdhc)!=0);
    // Unset STAT bits. These bits will reflect completion of command execution
    sdhc_stat_wr(&sdhc, ~0x0);
    assert (sdhc_stat_cc_rdf(&sdhc)==0x0);
    // Issue the command
    sdhc_ctm_index_wrf(&sdhc, 0x5);

    // XXX Does this detect if there is actually a card in the slot or just
    //     if the controller has the capability to read a card of that type?
    uint32_t cc, cto, run = 0;

    // Wait for completion
    do {
        cc = sdhc_stat_cc_rdf(&sdhc); 
        cto = sdhc_stat_cto_rdf(&sdhc); 
        run = 0;

        if (cc==0x1) {
            printf("SDIO card detected .. printing registers ..\n");
            assert (sdhc_stat_bada_rdf(&sdhc)==0x0);
        } else if (cto==0x1) {
            printf("No SDIO card detected .. \n");
        } else {
            run = 1;
        }
    } while(run);

    assert(sdhc_stat_bada_rdf(&sdhc)==0x0);

    // CMD line reset
    mmchs_cmd_line_reset();

    // Send a CMD8 command
    // CC value?          0x1 -> SD card
    // CMD8 needs an argument ..
    mmchs_send_cmd(CMD8, 0);
    do {
        if (sdhc_stat_cc_rdf(&sdhc)) {
            printf("SD card found .. (v 2.0 or higher)\n");        
            return;
        } else {
            printf("No SD card found .. \n");
        }
    } while (sdhc_stat_cto_rdf(&sdhc)!=0x1);

    // We do not check for other cards (i.e. SD v1.x, or unknown cards .. )
    assert(!"There is no SD v2 card in the slot .. \n");
}

/*
 * \brief Change the block frequency
 *
 * The maximum operating frequency is 50 MHz [PLS. page 1] 
 * TRM, Figure 24-48, page 5132
 * 
 * \param clkdiv Clockdivider value to use
 */
static void mmchs_change_clock_frequency(int clkdiv)
{
    // Disable clock
    sdhc_sysctl_cen_wrf(&sdhc, 0x0);

    // Set the frequency
    // The base frequency we get from the L3 interconnect is 96 MHz,
    // we want to be below 50 MHz, so we use 2 as a divider for the clock
    mmchs_set_frequency(clkdiv);

    // Wait for internal clock to be stable .. 
    while (!sdhc_sysctl_ics_rdf(&sdhc));

    // Enable SD card clock
    sdhc_sysctl_cen_wrf(&sdhc, 0x1);
}

/*
 * \brief Send a command using polling
 *
 * \param cmd_idx Index of the command to issue (e.g. 0 for CMD0)
 * \param arg Argument of the command, will be written to the ARG register.
 *    These are given as bits [8:39] in the Physical Layer spec (see page 25)
 *
 * TRM Figure 24-46, page 5129
 * HCS      3.7.1.1, page 107
 *          3.7.1.2, page 109
 *
 */
void mmchs_send_cmd(int cmd_idx, uint32_t arg)
{
    // Wait for previous command to finish
    int loop = 0;
    while (sdhc_ps_cmdi_rdf(&sdhc)!=0x0/*  ||  */
           /* sdhc_ps_DATI_rdf(&mmchs)!=0x0 */) { // HCS, 2.2.6
        // but DATI not mentioned in HCS 3.7.1.2
        // okay, depends on busy signal. Don't need to check if the busy signal is used

        mmchs_wait_msec(1);

        if(++loop>10000) {

            print_mmchs();
            assert(!"mmchs_send_cmd failed, CMDI seems to stick .. ");
        }
    }

    // FreeBSD: ti_mmchs.c, line 589
    // These values are only valid for MMC cards
    omap44xx_mmchs_CON_t con = omap44xx_mmchs_CON_rd(&mmchs);
    con = omap44xx_mmchs_CON_MIT_insert(con, 0x0);
    con = omap44xx_mmchs_CON_STR_insert(con, 0x0);
    // Written later .. 

    // XXX Set CSRE if response type permits
    //     see standard spec
    // This seems to be to detect some kind of card error
    // Don't think we need this, FreeBSD does not use it either

    // Write MMCHS_SYSCTL . DTO
    // DTO is the data timout counter
    // XXX CAPA_TCP is 0, does this mean that the timeout counter will not work?
    /* printf("Timeout counter base frequency: %d\n", sdhc_capa_TCF_rdf(&mmchs)); */
    /* printf("Timeout value is: %d\n", sdhc_sysctl_dto_rdf(&sdhc)); */
    sdhc_sysctl_dto_wrf(&sdhc, 0xe);

    // XXX Interrupt configuration
    //     Setting MMCHS_IE
    //     and MMCHS_ISE
    //     to enable interrupts
    /* sdhc_ie_wr(&sdhc, ~0x0); */
    /* sdhc_ise_wr(&sdhc, 0x0); */
    sdhc_ir_t ise = sdhc_ir_default;
    ise = sdhc_ir_cerr_insert(ise, 0x1);
    ise = sdhc_ir_cto_insert(ise, 0x1); 
    ise = sdhc_ir_cc_insert(ise, 0x1);
    ise = sdhc_ir_ceb_insert(ise, 0x1);
    // Written later .. 

    // Unset all bits in STAT
    sdhc_stat_wr(&sdhc, ~0x0);

    sdhc_ctm_t cmd = sdhc_ctm_default;

    // Configure command
    // For cmd8, which expects R7, .. 
    cmd = sdhc_ctm_index_insert(cmd, cmd_idx); // Command ID
    switch (cmd_idx) {
    case CMD8:
        // Free BSD: mmc_send_if_conf [mmc.c]
        // RSP_TYPE follows from MMC_CMD_BCR
        cmd = sdhc_ctm_rsp_type_insert(cmd, 0x2); // R7
        // MMC_RSP_R7 = MMC_RSP_PRESENT | MMC_RSP_CRC
        cmd = sdhc_ctm_dp_insert(cmd, 0x1); // Data present
        // Do a CRC check
        cmd = sdhc_ctm_ccce_insert(cmd, 0x1);
        ise = sdhc_ir_ccrc_insert(ise, 0x1);
        // Do index checking
        ise = sdhc_ir_cie_insert(ise, 0x1);
        break;
    case CMD0:
        cmd = sdhc_ctm_rsp_type_insert(cmd, 0x0); // R7
        break;
    default:
        assert(!"Unknown command .. ");
    }

    if (1) { // no data
        // XXX Write MMCHS_BLK
        //     Free BSD ti_mmchs.c l 631'ish
        //     for now, we don't have any data .. 
        sdhc_blckcnt_wr(&sdhc, 0x0);

        // Write CON
        omap44xx_mmchs_CON_wr(&mmchs, con);

        // Write IE (to enable internal generation of interrupts)
        // These are not actual interrupts
        sdhc_ie_wr(&sdhc, ise);
        
        // Write ISE (to enable interrupts passed to the system)
        // Enabling some interrupts I believe could be useful for debugging
        ise = sdhc_ir_default;
        ise = sdhc_ir_bada_insert(ise, 0x1); // Bad access 
        ise = sdhc_ir_dto_insert(ise, 0x1);  // Data timeout
        ise = sdhc_ir_cto_insert(ise, 0x1);  // Command timeout
        ise = sdhc_ir_cerr_insert(ise, 0x1); // Card error
        ise = sdhc_ir_dcrc_insert(ise, 0x1); // Data CRC error
        ise = sdhc_ir_ccrc_insert(ise, 0x1); // Command CRC err
        ise = sdhc_ir_cirq_insert(ise, 0x1); // Command CRC err
        ise = sdhc_ir_crem_insert(ise, 0x1); // Command CRC err
        ise = sdhc_ir_cins_insert(ise, 0x1); // Command CRC err
        sdhc_ise_wr(&sdhc, ise);

        // Write command argument
	sdhc_arg1_wr(&sdhc, arg);
        sdhc_ctm_wr(&sdhc, cmd);
    }
 
}

static int mmchs_finalize_cmd(void)
{
    volatile int32_t cto;
    volatile int32_t ccrc;
    volatile int32_t cc;

    int loop = 1; // << run loop until 0
    int cnt = 0;

    do {
        
        if (++cnt>1000) {

            printf("Timout for command, aborting .. \n");
            loop = 0;
            print_mmchs_state();
        }

        cto = sdhc_stat_cto_rdf(&sdhc); // < Command Timeout Error
        ccrc = sdhc_stat_ccrc_rdf(&sdhc);
        cc = sdhc_stat_cc_rdf(&sdhc);
        
        if (cto==0x1) { // << Timeout?
            loop = 0;
            mmchs_cmd_line_reset();
            return MMCHS_RESP_TIMEOUT;
        } else if (cc==0x1) { // << Command complete?
            sdhc_resp_pr(PRBUFL, &sdhc);
            sdhc_ctm_pr(PRBUFL, &sdhc);
            printf("CMD:\n%s\n", PRBUF);
            loop = 0;
            // Check for response ..
            if (sdhc_ctm_rsp_type_rdf(&sdhc)!=0x0) {
                // Read response
                printf("mmchs_send_cmd: CC - got a response! "
                       "Doing a mackerel print .. \n");
                sdhc_resp_pr(PRBUFL, &sdhc);
                // Verify error
                if (sdhc_stat_cie_rdf(&sdhc)!=0x0 ||
                    sdhc_stat_ceb_rdf(&sdhc)!=0x0 ||
                    sdhc_stat_ccrc_rdf(&sdhc)!=0x0 ||
                    sdhc_stat_cerr_rdf(&sdhc)!=0x0) {
                    
                    print_mmchs();
                    cm2_debug_print();
                    assert(!"mmchs_send_cmd: error - registers got dumped .. ");
                }
            } else {
                printf("mmchsd_send_cmd: no response \n");
            }
        }

        mmchs_wait_msec(1);

    } while(loop);
        
    printf("mmchs_send_cmd end (result after %d loop iterations) \n", cnt);
    return MMCHS_RESP_SUCCESS;
}

#ifdef MMCHS_DEBUG
/*
 * \brief Test register access
 *
 * Triest to write a couple of registers and reads them 
 * again to see if the reads return the changed value.
 *
 * I am doing this in a loop because I read somewhere that
 * the device might be idle and needs to wake up first (which
 * is triggered by the register access)
 *
 * STATE: This works for all but DVAL
 */
static void mmchs_test_register_write(void)
{
    printf("mmchs_test_register_write\n");

    for(int i=0; i<100; i++) {

        printf("HL standby is: %d - "
               "HL idle is: %d - "
               "DVAL is: %d - "
               "standby is: %d \n", 
               omap44xx_mmchs_HL_SYSCONFIG_STANDBYMODE_rdf(&mmchs),
               omap44xx_mmchs_HL_SYSCONFIG_IDLEMODE_rdf(&mmchs),
               omap44xx_mmchs_CON_DVAL_rdf(&mmchs),
               omap44xx_mmchs_SYSCONFIG_STANDBYMODE_rdf(&mmchs));

        omap44xx_mmchs_HL_SYSCONFIG_STANDBYMODE_wrf(&mmchs, 0x1);
        omap44xx_mmchs_HL_SYSCONFIG_IDLEMODE_wrf(&mmchs, 0x1);
        omap44xx_mmchs_SYSCONFIG_STANDBYMODE_wrf(&mmchs, 0x1);
        omap44xx_mmchs_CON_DVAL_wrf(&mmchs, 0x1);

        volatile int t = 0;
        do {
            t++;
        } while(t<CYCLES_PER_MSEC/10);

    }

    printf("mmchs_test_register_write .. DONE\n");
}
#endif

/*
 * \brief Print information on the power state of the MMC
 *
 * See SD Host Controller Spec, 1.10
 */
static void mmchs_print_power_state(void)
{
    printf("Internal clock enable: %d\n", sdhc_sysctl_ice_rdf(&sdhc));
    printf("Card Inserted: %d\n", sdhc_ps_cins_rdf(&sdhc));
    printf("SD Bus Power: %d\n", sdhc_hctl_sdbp_rdf(&sdhc));
    printf("SD Clock Enable: %d\n", sdhc_sysctl_cen_rdf(&sdhc));
    printf("Command Inhibit: %d\n", sdhc_ps_cmdi_rdf(&sdhc));
    printf("DAT inhibit: %d\n", sdhc_ps_dati_rdf(&sdhc));
}

/*
 * \brief Disable standby mode
 *
 * Figured this one out by reading descriptions of device regiters ..
 */
 static void mmchs_disable_standby(void) 
{
    // This is not part of the official initialization flow documentation
    omap44xx_mmchs_HL_SYSCONFIG_STANDBYMODE_wrf(&mmchs, 0x1);
    omap44xx_mmchs_HL_SYSCONFIG_IDLEMODE_wrf(&mmchs, 0x1);
    omap44xx_mmchs_SYSCONFIG_STANDBYMODE_wrf(&mmchs, 0x1);
}

/*
 * \brief Enable wakupe of the host controller
 *
 * OMAP TRM 24.5.1.1.2.4
 */
static void mmchs_enable_wakeup(void)
{
    omap44xx_mmchs_SYSCONFIG_ENAWAKEUP_wrf(&mmchs, 0x1);
    sdhc_hctl_iwe_wrf(&sdhc, 0x1);
}

/*
 * \brief Detect if there is a card in the card reader
 *
 * SD card spec 3.1
 * This is not working! Maybe we do need interrupt for that?
 */
static void mmchs_detect_card(void)
{
    sdhc_stat_wr(&sdhc, ~0x0);
    assert(sdhc_stat_cirq_rdf(&sdhc)==0x0);

    // Read old interrupt configuration
    sdhc_ir_t ie = sdhc_ie_rd(&sdhc);
    sdhc_ir_t ise = sdhc_ise_rd(&sdhc);
    // Enable detection for insertion of card .. 
    ie =  sdhc_ir_cins_insert(ie, 0x1);
    ie =  sdhc_ir_crem_insert(ie, 0x1);
    ise = sdhc_ir_crem_insert(ise, 0x1);
    ise = sdhc_ir_cins_insert(ise, 0x1);
    // Write new interrupt configuration
    sdhc_ie_wr(&sdhc, ie);
    sdhc_ise_wr(&sdhc, ise);

    printf("Waiting for card to be inserted .. \n");
    //sdhc_pr(PRBUFL, &sdhc);
    //printf("%s\n", PRBUF);

    //sdhc_ise_pr(PRBUFL, &sdhc);
    //printf("%s\n", PRBUF);
    //sdhc_ie_pr(PRBUFL, &sdhc);
    //printf("%s\n", PRBUF);

     int i = 0;
     while (sdhc_ps_cins_rdf(&sdhc)!=0x1) {

         if (++i>10000) {
             printf("No card detected .. \n");
             return;
         }

         mmchs_wait_msec(1);
     }

     printf("Card detected .. \n");
      mmchs_init_stream();
     mmchs_identify_card();
}

/*
 * \brief Prepare configuration of MMC host controller
 *
 */
static void mmchs_pre_configure(void) 
{
    printf("mmchs: pre_configure()\n");

    // need connection to TWL6030 for sdmmc1_enable_power()
    ti_twl6030_init();
    // for testing
    ti_twl6030_vmmc_pr();

    // TRM chapter 18: Control module? Is that CM2?
    sdmmc1_enable_power();

    // ==================================================
    // STEP 2: Software reset
    mmchs_do_sw_reset();
    mmchs_do_state_machine_reset();
    
    // ==================================================
    // Step 1b: Disable idle and stanbdy states
    mmchs_disable_standby();
    mmchs_enable_wakeup();
}

/*
 * \brief Configure and initialize MMC
 *
 * TRM Section 24.5.1.1.2 on page 5114 ff
 * and BSD code ti_mmchs_hw_init in ti_mmchs.c
 *
 */
static void mmchs_configure(void) 
{
    // ==================================================
    // STEP 3: Set module hardware capabilities
    // Need to write MMCHS_CAPA[26:24] ..
    // BSD: ti_mmchs_hw_init, line 1373
    sdhc_capa_t capa = sdhc_capa_rd(&sdhc);
    capa = sdhc_capa_vs18_insert(capa, 0x1);
    capa = sdhc_capa_vs30_insert(capa, 0x1);
#if defined(MMCHS_VS33)
    // According to the Spec, the card should support 2.7-3.6V
    // chp 3.2 in PLS
    capa = sdhc_capa_vs33_insert(capa, 0x1); // < NOT done by Free BSD
#endif
    sdhc_capa_wr(&sdhc, capa);
    // .. and MMCHS_CUR_CAPA[23:0]
    // This is not done by BSD code either!
    // Also: a comment in the TRM says that this is not implemented

    // MMC host and bust configuration
    // TRM: see Figure 24-37, page 5116
    // BSD: ti_mmchs_hw_init, line 1375, point 5
    omap44xx_mmchs_CON_t con = omap44xx_mmchs_CON_rd(&mmchs);
    // Set a couple of default values
    con = omap44xx_mmchs_CON_OD_insert(con, 0x0);
    con = omap44xx_mmchs_CON_DW8_insert(con, 0x0);
    con = omap44xx_mmchs_CON_CEATA_insert(con, 0x0); // Standard, not ATA mode
    // Written later

    // Make sure SD bus power is not yet turned on
    assert(sdhc_hctl_sdbp_rdf(&sdhc)==0x0);

    // Get old configuration
    sdhc_hctl_t hctl = sdhc_hctl_rd(&sdhc);

    // Step 4
    // ==================================================
    // It seems that all cards need to support a bus width of 1
    // SD card spec v1
    // This is from the Free BSD code
    uint32_t bus_width = 1;
    switch (bus_width) {
    case 1:
        printf("mmchs: setting bus width to 1\n");
        hctl = sdhc_hctl_dtw_insert(hctl, 0x0); // Data transfer width 0=1 Bit
        con = omap44xx_mmchs_CON_DW8_insert(con, 0x0); // must be 0 for SD cards
        break;
    case 4:
        printf("mmchs: setting bus width to 4\n");
        hctl = sdhc_hctl_dtw_insert(hctl, 0x1); // Data transfer width 1=4 Bit
        con = omap44xx_mmchs_CON_DW8_insert(con, 0x0); // must be 0 for SD cards
        break;
    case 8:
        assert(!"DW8=1 (a bus width of 8) is not supported for SD cards");
        con = omap44xx_mmchs_CON_DW8_insert(con, 0x1);
        break;
    default:
        assert(!"Given bus width not supported\n");
    }
    
    // Write configuration
    omap44xx_mmchs_CON_wr(&mmchs, con);

    // Step 5
    // ==================================================
    // XXX There seems to be a lot of redundancy with the previous setup ..
#if defined(MMCHS_VS33)
    hctl = sdhc_hctl_sdvs_insert(hctl, 0x7); // 0x7=3.3V
#else
    hctl = sdhc_hctl_sdvs_insert(hctl, 0x6); // 0x6=3.0V, 0x5=1.8V
#endif
    hctl = sdhc_hctl_sdbp_insert(hctl, 0x0); // Disable SD bus power
    sdhc_hctl_wr(&sdhc, hctl);

    // XXX Something happening with the TWL now, see BSE @ 1032
    // Just ignore this for now

    // Power on the BUS
    sdhc_hctl_sdbp_wrf(&sdhc, 0x1);

    // Wait for SDBP to come up
    printf("mmchsh: turning on SD bus power .. ");
    // Wait for init to be done
    uint32_t sdbp_loop = 0;
    while (sdhc_hctl_sdbp_rdf(&sdhc)==0x0) {
        if (++sdbp_loop>1000) {
            printf("\n");
            print_mmchs();
            assert(!"Timeout in setting SDBP");
        }
        mmchs_wait_msec(1);
    }
    printf("done (loop=%d)\n", sdbp_loop);

    // Enable internal clock
    // TRM Figure 24-37
    sdhc_sysctl_ice_wrf(&sdhc, 0x1);

    // Clock configuration
    mmchs_set_frequency(0x3FF);

    // BSD: get the clock from omap4_clk_hsmmc_get_source_freq
    //          which is in arm/ti/omap4/omap4_prcm_clks.c
    //          which then calls into omap4_clk_details
    //          which returns 64 MHz or 96 MHz
    
    // Could not yet figure out what ios->clock is supposed to be ..
    // Let's assume it is 1 ..
    // In that case we need to divide by 1200 to get 80 kHz
    // The closest we can get is 1023 ....

    // Waiting for internal clock to become stable
    int ics_loop = 0;
    while (sdhc_sysctl_ics_rdf(&sdhc)!=1) {
        ics_loop++;
    }
    printf("mmchs: internal clock stable .. %d loops\n", ics_loop);

    // 0x3 means MMC1_ICLK=on and MMC1_FCLK=1 (see Table 24-9)
    omap44xx_mmchs_SYSCONFIG_t sysconfig = omap44xx_mmchs_SYSCONFIG_rd(&mmchs);
    /* sysconfig = omap44xx_mmchs_SYSCONFIG_AUTOIDLE_insert(sysconfig, 0x0); */
    /* sysconfig = omap44xx_mmchs_SYSCONFIG_SIDLEMODE_insert(sysconfig, 0x1); */
    sysconfig = omap44xx_mmchs_SYSCONFIG_CLOCKACTIVITY_insert(sysconfig, 0x3);
    omap44xx_mmchs_SYSCONFIG_wr(&mmchs, sysconfig);
    int clkactiv = 0;
    while (omap44xx_mmchs_SYSCONFIG_CLOCKACTIVITY_rdf(&mmchs)!=0x3) {
        clkactiv++;
    }
    printf("mmchs: clock acitivity enabled .. %d loops\n", clkactiv);

    printf("mmchs: configuration done\n");
    // EOF Figure 24-37
}

void mmchs_init(void)
{
    cm2_init();

    lvaddr_t mmchs_vaddr;
    errval_t err = map_device_register(MMCHS_BASE, 0x1000, &mmchs_vaddr);
    assert(err_is_ok(err));
    
    // Initialize devices
    omap44xx_mmchs_initialize(&mmchs, (mackerel_addr_t)mmchs_vaddr);
    sdhc_initialize(&sdhc, (mackerel_addr_t) mmchs_vaddr + 0x200);
    ctrlmod_init();

    printf("\nmmchs: entered init().\n");

    // Enable interrupts
    err = inthandler_setup_arm(mmchs_handle_irq, NULL, MMC1_IRQ);
    assert(err_is_ok(err));

    // Configure Pad multiplexing
    // Does not change anything ctrlmod_init();

    // Enable power
    cm2_enable_hsmmc1();

    // Configure device
    mmchs_pre_configure();
    mmchs_configure();

    /* // Change clock frequency to 50 MHz (as Linux) */
    /* mmchs_change_clock_frequency(0x2); */
    /* mmchs_do_state_machine_reset(); */

    switch (sdhc_rev_srev_rdf(&sdhc)) {
    case 0x0:
        printf("SD Host Specification Version 1.0\n");
        break;
    case 0x1:
        printf("SD Host Specification Version 2.0\n");
        break;
    default:
        assert(!"Don't understand SREV field");
    }
    

    mmchs_init_stream();
    mmchs_detect_card();
    /* mmchs_init_and_ident_card(); */


    /* printf("mmchs_detect_card\n"); */
    /* /\* mmchs_detect_card(); *\/ */
    /* sdhc_ie_wr(&sdhc, ~0x0); */
    /* sdhc_ise_wr(&sdhc, ~0x0); */
    
    /* mmchs_print_power_state(); */

    /* int resp; */
    /* int loop = 0; */

    /* do { */

    /*     printf("Trying to init card, step %d\n", ++loop); */
        
    /*     /\* mmchs_wait_msec(100); *\/ */
        
    /*     /\* // See diagram 24-48 for details on how to set card frequency *\/ */
    /*     /\* mmchs_change_clock_frequency(); *\/ */

    /*     mmchs_wait_msec(100); */
    /*     printf("Init and ident card .. \n"); */
    /*     resp = mmchs_init_and_ident_card(); */

    /*     mmchs_wait_msec(1000); */

    /* } while(resp!=MMCHS_RESP_SUCCESS && loop<100); */

}

/*
 * \brief Interrupt handler for MMC
 *
 * See TRM 24.4.4.1, page 5092
 */
void mmchs_handle_irq(void *args)
{
    printf("mmchs: got interrupt\n");

    cm2_print_standby_state();
    sdhc_stat_t stat = sdhc_stat_rd(&sdhc);

    sdhc_stat_pr(PRBUFL, &sdhc);
    printf("%s\n", PRBUF);

    // Handle interrupt
    if (sdhc_stat_bada_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt BADA\n");
    }
    else if (sdhc_stat_cerr_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt CERR\n");
    }
    else if (sdhc_stat_admae_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt ADMAE\n");
    }
    else if (sdhc_stat_ace_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt ACE\n");
    }
    else if (sdhc_stat_deb_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt DEB\n");
    }
    else if (sdhc_stat_dcrc_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt DCRC\n");
    }
    else if (sdhc_stat_dto_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt DTO\n");
    }
    else if (sdhc_stat_cie_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt CIE\n");
    }
    else if (sdhc_stat_ceb_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt CEB\n");
    }
    else if (sdhc_stat_ccrc_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt CCRC\n");
    }
    else if (sdhc_stat_cto_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt CTO\n");
    }
    else if (sdhc_stat_erri_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt ERRI\n");
    }
    else if (sdhc_stat_intb_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt BSR\n");
    }
    else if (sdhc_stat_inta_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt OBI\n");
    }
    else if (sdhc_stat_cirq_extract(stat)==0x1) {
        sdhc_ise_cirq_wrf(&sdhc, 0x0);
        sdhc_ie_cirq_wrf(&sdhc, 0x0);
        printf("Implement mmch interrupt handler action for interrupt CIRQ\n");
    }
    else if (sdhc_stat_crem_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt CREM\n");
    }
    else if (sdhc_stat_cins_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt CINS\n");
    }
    else if (sdhc_stat_brr_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt BRR\n");
    }
    else if (sdhc_stat_bwr_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt BWR\n");
    }
    else if (sdhc_stat_dma_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt DMA\n");
    }
    else if (sdhc_stat_bge_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt BGE\n");
    }
    else if (sdhc_stat_tc_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt TC\n");
    }
    else if (sdhc_stat_cc_extract(stat)==0x1) {
        printf("Implement mmch interrupt handler action for interrupt CC\n");
    }

    // Unset STAT bits
    sdhc_stat_wr(&sdhc, ~0x0);
    
    printf("%s:%d: Never return ..\n", __FUNCTION__, __LINE__);
    while (1) ;
}


int main(int argc, char** argv) 
{
    mmchs_init();
    return 0;
}