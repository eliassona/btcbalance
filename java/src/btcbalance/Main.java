package btcbalance;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class Main {
	private static final String CLOJURE_CORE = "clojure.core";
	private static final String BTCBALANCE_CORE = "btcbalance.core";
	private static final String BTCBALANCE_ASSETS = "btcbalance.secure.assets";

	public static void main(final String[] args) {
		final IFn require = Clojure.var(CLOJURE_CORE, "require");
		
		try {
			require.invoke(Clojure.read(BTCBALANCE_CORE));
			final IFn save = Clojure.var(BTCBALANCE_CORE, "save!");
			save.invoke();
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		require.invoke(Clojure.read(BTCBALANCE_ASSETS));
		final IFn saveRates = Clojure.var(BTCBALANCE_ASSETS, "save-rates!");
		saveRates.invoke();

	}
}
