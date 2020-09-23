package com.g2forge.reassert.express.model;

import com.g2forge.alexandria.java.function.ISupplier;

public interface IExplained<Value> extends ISupplier<Value> {
	public enum Relevance {
		Unevaluated,
		Identity,
		Combined,
		Dominant;
	}
}
