package scorex.transaction.state

import scorex.transaction.{AccountTransactionsHistory, BalanceSheet, State}

trait LagonakiState extends State with BalanceSheet with AccountTransactionsHistory
