package com.g2forge.reassert.core.api.module.config;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.type.ref.ITypeRef;

public interface IConfig {
	public <T> IOptional<T> load(ITypeRef<T> type);
}
