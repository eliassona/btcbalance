package btcbalance;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class Main {
	private static final String CLOJURE_CORE = "clojure.core";
	private static final String BTCBALANCE_CORE = "btcbalance.core";
	private static final String BTCBALANCE_ANDERS = "btcbalance.secure.anders";

	public static void main(final String[] args) {
		final IFn require = Clojure.var(CLOJURE_CORE, "require");
		require.invoke(Clojure.read(BTCBALANCE_CORE));
		require.invoke(Clojure.read(BTCBALANCE_ANDERS));
		final IFn save = Clojure.var(BTCBALANCE_CORE, "save!");
		save.invoke();
		final IFn saveRates = Clojure.var(BTCBALANCE_ANDERS, "save-rates!");
		saveRates.invoke();

	}
}
