/*
  Register definitions for slave core: CBMIA 

  * File           : cbmia_regs.h
  * Author         : auto-generated by wbgen2 from cbmia_regs.wb
  * Created        : Wed Feb 29 08:44:58 2012
  * Standard       : ANSI C

    THIS FILE WAS GENERATED BY wbgen2 FROM SOURCE FILE cbmia_regs.wb
    DO NOT HAND-EDIT UNLESS IT'S ABSOLUTELY NECESSARY!

*/

#ifndef __WBGEN2_REGDEFS_CBMIA_REGS_WB
#define __WBGEN2_REGDEFS_CBMIA_REGS_WB

#include <inttypes.h>

#if defined( __GNUC__)
#define PACKED __attribute__ ((packed))
#else
#error "Unsupported compiler?"
#endif

#ifndef __WBGEN2_MACROS_DEFINED__
#define __WBGEN2_MACROS_DEFINED__
#define WBGEN2_GEN_MASK(offset, size) (((1<<(size))-1) << (offset))
#define WBGEN2_GEN_WRITE(value, offset, size) (((value) & ((1<<(size))-1)) << (offset))
#define WBGEN2_GEN_READ(reg, offset, size) (((reg) >> (offset)) & ((1<<(size))-1))
#define WBGEN2_SIGN_EXTEND(value, bits) (((value) & (1<<bits) ? ~((1<<(bits))-1): 0 ) | (value))
#endif


/* definitions for register: Control register. */

/* definitions for field: Software reset. in reg: Control register. */
#define CBMIA_CTRL_SW_RST                     WBGEN2_GEN_MASK(0, 1)

/* definitions for field: Send frame. in reg: Control register. */
#define CBMIA_CTRL_SEND_FRAME                 WBGEN2_GEN_MASK(1, 1)

/* definitions for register: Global status register. */

/* definitions for field:  in reg: Global status register. */
#define CBMIA_STAT_                           WBGEN2_GEN_MASK(0, 1)

/* definitions for field:  in reg: Global status register. */
#define CBMIA_STAT_                           WBGEN2_GEN_MASK(1, 1)

/* definitions for register: Interrupt disable register */

/* definitions for field: mil1553 frame received. in reg: Interrupt disable register */
#define CBMIA_EIC_IDR_RX_DONE                 WBGEN2_GEN_MASK(0, 1)

/* definitions for register: Interrupt enable register */

/* definitions for field: mil1553 frame received. in reg: Interrupt enable register */
#define CBMIA_EIC_IER_RX_DONE                 WBGEN2_GEN_MASK(0, 1)

/* definitions for register: Interrupt mask register */

/* definitions for field: mil1553 frame received. in reg: Interrupt mask register */
#define CBMIA_EIC_IMR_RX_DONE                 WBGEN2_GEN_MASK(0, 1)

/* definitions for register: Interrupt status register */

/* definitions for field: mil1553 frame received. in reg: Interrupt status register */
#define CBMIA_EIC_ISR_RX_DONE                 WBGEN2_GEN_MASK(0, 1)

/* definitions for register: FIFO 'mil1553 transmitter FIFO' data input register 0 */

/* definitions for field: mil1553 word to transmit in reg: FIFO 'mil1553 transmitter FIFO' data input register 0 */
#define CBMIA_TX_FIFO_R0_WORD_MASK            WBGEN2_GEN_MASK(0, 16)
#define CBMIA_TX_FIFO_R0_WORD_SHIFT           0
#define CBMIA_TX_FIFO_R0_WORD_W(value)        WBGEN2_GEN_WRITE(value, 0, 16)
#define CBMIA_TX_FIFO_R0_WORD_R(reg)          WBGEN2_GEN_READ(reg, 0, 16)

/* definitions for field: mil1553 word type flag.
0 = data
1 = command in reg: FIFO 'mil1553 transmitter FIFO' data input register 0 */
#define CBMIA_TX_FIFO_R0_WORD_TYPE            WBGEN2_GEN_MASK(16, 1)

/* definitions for register: FIFO 'mil1553 transmitter FIFO' control/status register */

/* definitions for field: FIFO full flag in reg: FIFO 'mil1553 transmitter FIFO' control/status register */
#define CBMIA_TX_FIFO_CSR_FULL                WBGEN2_GEN_MASK(16, 1)

/* definitions for field: FIFO empty flag in reg: FIFO 'mil1553 transmitter FIFO' control/status register */
#define CBMIA_TX_FIFO_CSR_EMPTY               WBGEN2_GEN_MASK(17, 1)

/* definitions for field: FIFO counter in reg: FIFO 'mil1553 transmitter FIFO' control/status register */
#define CBMIA_TX_FIFO_CSR_USEDW_MASK          WBGEN2_GEN_MASK(0, 6)
#define CBMIA_TX_FIFO_CSR_USEDW_SHIFT         0
#define CBMIA_TX_FIFO_CSR_USEDW_W(value)      WBGEN2_GEN_WRITE(value, 0, 6)
#define CBMIA_TX_FIFO_CSR_USEDW_R(reg)        WBGEN2_GEN_READ(reg, 0, 6)

/* definitions for register: FIFO 'mil1553 receiver FIFO' data output register 0 */

/* definitions for field: mil1553 word received in reg: FIFO 'mil1553 receiver FIFO' data output register 0 */
#define CBMIA_RX_FIFO_R0_WORD_MASK            WBGEN2_GEN_MASK(0, 16)
#define CBMIA_RX_FIFO_R0_WORD_SHIFT           0
#define CBMIA_RX_FIFO_R0_WORD_W(value)        WBGEN2_GEN_WRITE(value, 0, 16)
#define CBMIA_RX_FIFO_R0_WORD_R(reg)          WBGEN2_GEN_READ(reg, 0, 16)

/* definitions for field: mil1553 word type flag.
0 = data
1 = status in reg: FIFO 'mil1553 receiver FIFO' data output register 0 */
#define CBMIA_RX_FIFO_R0_WORD_TYPE            WBGEN2_GEN_MASK(16, 1)

/* definitions for field: mil1553 word error flag.
0 = no error
1 = word contains error in reg: FIFO 'mil1553 receiver FIFO' data output register 0 */
#define CBMIA_RX_FIFO_R0_WORD_ERROR           WBGEN2_GEN_MASK(17, 1)

/* definitions for field: Parity error flag.
0 = no error
1 = parity error in reg: FIFO 'mil1553 receiver FIFO' data output register 0 */
#define CBMIA_RX_FIFO_R0_PARITY_ERROR         WBGEN2_GEN_MASK(18, 1)

/* definitions for field: Manchester code violation flag.
0 = no violation
1 = code violation in reg: FIFO 'mil1553 receiver FIFO' data output register 0 */
#define CBMIA_RX_FIFO_R0_CODE_VIOLATION       WBGEN2_GEN_MASK(19, 1)

/* definitions for register: FIFO 'mil1553 receiver FIFO' control/status register */

/* definitions for field: FIFO full flag in reg: FIFO 'mil1553 receiver FIFO' control/status register */
#define CBMIA_RX_FIFO_CSR_FULL                WBGEN2_GEN_MASK(16, 1)

/* definitions for field: FIFO empty flag in reg: FIFO 'mil1553 receiver FIFO' control/status register */
#define CBMIA_RX_FIFO_CSR_EMPTY               WBGEN2_GEN_MASK(17, 1)

/* definitions for field: FIFO counter in reg: FIFO 'mil1553 receiver FIFO' control/status register */
#define CBMIA_RX_FIFO_CSR_USEDW_MASK          WBGEN2_GEN_MASK(0, 6)
#define CBMIA_RX_FIFO_CSR_USEDW_SHIFT         0
#define CBMIA_RX_FIFO_CSR_USEDW_W(value)      WBGEN2_GEN_WRITE(value, 0, 6)
#define CBMIA_RX_FIFO_CSR_USEDW_R(reg)        WBGEN2_GEN_READ(reg, 0, 6)

PACKED struct CBMIA_WB {
  /* [0x0]: REG Control register. */
  uint32_t CTRL;
  /* [0x4]: REG Global status register. */
  uint32_t STAT;
  /* padding to: 8 words */
  uint32_t __padding_0[6];
  /* [0x20]: REG Interrupt disable register */
  uint32_t EIC_IDR;
  /* [0x24]: REG Interrupt enable register */
  uint32_t EIC_IER;
  /* [0x28]: REG Interrupt mask register */
  uint32_t EIC_IMR;
  /* [0x2c]: REG Interrupt status register */
  uint32_t EIC_ISR;
  /* [0x30]: REG FIFO 'mil1553 transmitter FIFO' data input register 0 */
  uint32_t TX_FIFO_R0;
  /* [0x34]: REG FIFO 'mil1553 transmitter FIFO' control/status register */
  uint32_t TX_FIFO_CSR;
  /* [0x38]: REG FIFO 'mil1553 receiver FIFO' data output register 0 */
  uint32_t RX_FIFO_R0;
  /* [0x3c]: REG FIFO 'mil1553 receiver FIFO' control/status register */
  uint32_t RX_FIFO_CSR;
};

#endif