 # testTransferApis

This is a simple script that does one thing: it tests api from project transf_api

It is an sbt project that can be run by simply doing 

sbt assebly 

from root directory and then by 

java -cp java -cp target/scala-2.10/testTransferApis-assembly-1.0.jar TestTransfer

It must be run only when Play application from transf_api is running, it will query the localhost.

Assuming that the original application was just started, this script populates data store with some clients and accounts
and then demonstrates how the money can be sent from customer to cosutomer, and what happens in diffent situations (successful
transfer, not enough money etc).

To illustrate what happened the script prins information about accounts and clients involved and shows balances before and
after transcation. It also prints out generated jsom messages that it sends.

