package com.g2forge.reassert.express.v2.model;

import com.g2forge.alexandria.java.function.ISupplier;

public interface IExplained<Value> extends ISupplier<Value> {
	public enum Relevance {
		Unevaluated,
		Evaluated,
		Dominant;
	}
}
