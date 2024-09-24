package btcbalance;

import java.util.Date;
import java.util.List;
import java.util.Map;

import clojure.lang.Keyword;
import clojure.lang.Symbol;

public class HistoricalRate {
	private final List<?> deposits;
	public HistoricalRate(final List<?> deposits) {
		this.deposits = deposits;
	}
	public Date depoDateOf(final Date txDate) {
		for (int i = deposits.size() - 1; i >= 0; i--) {
			final Date result = depoDateOf(txDate, (Map) deposits.get(i));
			if (result != null)  {
				return result;
			}
		}
		throw new IllegalArgumentException();
	}
	private Date depoDateOf(final Date txDate, final Map m) {
		final Date depoDate = (Date) m.get(kwOf("date"));
		if (txDate.getTime() > depoDate.getTime()) {
			return depoDate;
		}
		return null;
	}
	
	public static Keyword kwOf(final String s) {
		return Keyword.intern(Symbol.create(s));
	}
	
	public static void main(final String[] args) {
		System.out.println(Keyword.intern(Symbol.create("date")));
	}
}
