def initialize(context): 
    context.benchmarkSecurity = symbol("SPY")
    set_commission(commission.PerShare(cost=0.0))
    context.sec_counts = np.array([5, 5])
    context.leverage = 2.0
    context.trade_manager = EventManager(period = 15, max_daily_hits = 1, rule_func = lambda x: True) 

def initialize(context):
    # Rebalance weekly on the first day of the week at market open
    schedule_function(rebalance,
                      date_rule=date_rules.week_start(days_offset=1),
                      time_rule=time_rules.market_open())

    attach_pipeline(make_pipeline(), 'fundamentals_pipeline')

def make_pipeline():

    # Latest p/e ratio.
    pe_ratio = Fundamentals.pe_ratio.latest
    
    # Number of days since the fundamental data was updated. In this example, we consider the p/e ratio
    # to be fresh if it's less than 4 days old.
    is_fresh = (BusinessDaysSincePreviousEvent(inputs=[Fundamentals.pe_ratio_asof_date]) <= 4)
    
    # Other indicators
    market_c = Fundamentals.market_cap.latest
    shares_o = Fundamentals.shares_outstanding.latest
    basic_e = Fundamentals.basic_eps.latest
    fcf = Fundamentals.free_cash_flow.latest

    # QTradableStocksUS is a pre-defined universe of liquid securities. 
    universe = QTradableStocksUS() & is_fresh

    # Filter
    temp = market_c.filter()
    # Top 50 and bottom 50 stocks ranked by p/e ratio
    top_pe_stocks = pe_ratio.top(100, mask=universe)
    bottom_pe_stocks = pe_ratio.bottom(100, mask=universe)
    
    # Screen to include only securities tradable for the day
    securities_to_trade = (top_pe_stocks | bottom_pe_stocks)
    
    pipe = Pipeline(
              columns={
                'pe_ratio': pe_ratio,
                'longs': top_pe_stocks,
                'shorts': bottom_pe_stocks,
              },
              screen = securities_to_trade
          )

    return pipe

def before_trading_start(context, data):
    num_stocks = 100
    fundamental_df = get_fundamentals(
        query(
            fundamentals.valuation.market_cap,
        )
        .filter(fundamentals.valuation.market_cap != None)
        .filter(fundamentals.valuation.shares_outstanding != None) 
        .filter(fundamentals.valuation.market_cap > 1e8)
        .filter(fundamentals.earnings_reprots.basic_eps > 0) 
        .filter(fundamentals.cash_flow_statement.free_cash_flow > 0)
        .order_by(fundamentals.valuation.market_cap.desc())
        .limit(num_stocks)
    )
    update_universe(fundamental_df)

def handle_data(context, data):
    dt = get_datetime()
    if not context.trade_manager.signal(dt):
        return
    df = history(200, "1d", "price")
    diff = df.tail(50).mean()/df.mean() - 1
    diff = diff.dropna()
    diff.sort()
    
    buys = diff[diff > 0.05]
    sells = diff[diff < -0.05]
    
    # add weights 
    counts = np.array([len(buys), len(sells)], dtype = float)
    weights = (context.leverage / context.sec_counts)*counts/sum(counts)
    
    # sort these 
    buys.sort()
    sells.sort()
    
    buys = buys.head(context.sec_counts[0])
    sells = sells.tail(context.sec_counts[1])
    
    stops = df.iloc[-1]*0.02
    







from quantopian.pipeline import Pipeline
from quantopian.pipeline.experimental import QTradableStocksUS

def make_pipeline():

    return Pipeline(
        columns={
            # Your pipeline columns go here.
        },
        screen=QTradableStocksUS()
    )


import quantopian.algorithm as algo
import quantopian.optimize as opt

MAX_SHORT_POSITION_SIZE = 0.01  # 1%
MAX_LONG_POSITION_SIZE = 0.01   # 1%

# Define the position concentration constraint.
constrain_pos_size = opt.PositionConcentration.with_equal_bounds(
    -MAX_SHORT_POSITION_SIZE,
    MAX_LONG_POSITION_SIZE,
)

# Supply the constraint to order_optimal_portfolio.
algo.order_optimal_portfolio(
    objective=my_objective, #Fill in with your objective function.
    constraints=[
        constrain_pos_size,
    ],
)
