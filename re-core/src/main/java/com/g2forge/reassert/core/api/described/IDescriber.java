package com.g2forge.reassert.core.api.described;

import com.g2forge.alexandria.java.type.ref.ITypeRef;

public interface IDescriber<T> {
	public IDescription describe(T value);

	public ITypeRef<T> getType();
}
