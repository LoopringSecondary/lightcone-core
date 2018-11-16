
# 关于

## TODO(dongw)

- [ ] optimize MarketManagerImpl
- [ ] optimize AccountTokenManagerImpl

## 以太坊块数据的处理

整个系统对以太坊上面路印的所有交易数据都需要做处理，进而调整pending ring和order的状态。以太坊区块里面的路印交易数据包括两类，一类是失败的环路，一类是成功的环路。对于这两种不同的环路，调用`lightcone-core`的方式不一样：

### 失败环路的处理

我们首先需要计算环路的hash，然后调用`PendingRingPool`的`removeRing`方法将这个环路从pool里面删掉。

1.  有可能在调用的时候`PendingRingPool`找不到有这个hash的环路(`removeRing`返回`false`)，这说明该环路是由其他的relay提交的。这种情况下，`PendingRingPool`的数据不应该变化。我们也没有必要对该环路里面的订单数据做进一步处理。


2。 如果能找到这个环路，说明环路是由我们的中继提交的，那么就需要释放相关订单被该环路占用的`pendingAmountS`，同样我们也没有必要对该环路里面的订单数据做进一步处理。


### 成功环路的处理

对于成功的环路，我们必须意识到：

- `PendingRingPool`对每个订单`pendingAmountS`的预估可能是不准确的。
- 考虑到未来可能的流动性共享，即使该环路不是我们提交的，也需要也对该环路里面的每个订单的实际大小（通过`AccountManager.adjustOrder`）做重新的计算，因为其中有的订单可能是本系统也维护的。对不存在的订单调用`AccountManager.adjustOrder`不会对`AccountManager`的数据有任何的改变。



因此对于成功的环路，计算环路的hash，然后调用`PendingRingPool`的`removeRing`方法将这个环路从pool里面删掉。

1. 如果在调用的时候`PendingRingPool`找不到有这个hash的环路，说明环路不是我们提交的。这时候`PendingRingPool`的数据不会有变化。我们对环路里面的每个订单计算hash值，然后调用协议的方法找到该订单已经成交的`filledAmountS`，然后再调用`AccountManager.adjustOrder`，如果`AccountManager.adjustOrder`返回`false`，说明该订单不是我们的，就可以不做任何处理；如果`AccountManager.adjustOrder`返回`true`,那么将该订单通过`MarketManager.submitOrder`**重新提交**。 （参考：订单提交和再提交）

2. 如果在调用的时候`PendingRingPool`能找的到该环路，那么就将这个环路的信息从`PendingRingPool`删掉，并且不要释放相关订单的`pendingAmountS`，进而防止在处理该环路订单之前，这些订单的可交易值变大，被系统撮合了。接下来对订单大小也要通过调用`AccountManager.adjustOrder`重新计算和**再次提交**。


## 订单提交和再提交

由于订单的可交易量可能有阶段性变化，一个订单可能被需要被提交几次到`MarketManager`。`MarketManager`总是假设该订单可能已经存在。因此在每次提交的时候，需将该订单先删掉，然后再添加。在这种情况下的删除订单，应该不要改变该订单相关的`PendingRingPool`信息。


## 一些情况的考虑

1. 如果一个订单A，A.amountS = 100, 我们的一个pendign的环路X，占用了20（A.pendingAmountS = 20）。这时候我们接到了一个成功清算的第三方环路Y，里面的成交是35。我们对Y进行了上述的处理，这时候在我们的系统里，订单的实际大小应该是65，但我们的环路还没有处理结果，这时候系统内该订单的实际可交易大小应该是65-20=45.

