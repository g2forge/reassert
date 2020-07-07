package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.adt.name.IStringNamed;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;

import lombok.Getter;

public class StringNamedDescriber implements IDescriber<IStringNamed>, ISingleton {
	protected static final StringNamedDescriber INSTANCE = new StringNamedDescriber();

	public static StringNamedDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<IStringNamed> type = ITypeRef.of(IStringNamed.class);

	protected StringNamedDescriber() {}

	@Override
	public IDescription describe(IStringNamed value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return value.getName();
			}

			@Override
			public String getName() {
				return value.getName();
			}
		};
	}
}