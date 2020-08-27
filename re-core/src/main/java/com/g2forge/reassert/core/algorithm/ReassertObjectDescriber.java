package com.g2forge.reassert.core.algorithm;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.alexandria.java.type.function.TypeSwitch1.FunctionBuilder;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;

import lombok.AccessLevel;
import lombok.Getter;

public class ReassertObjectDescriber implements IFunction1<Object, IDescription> {
	@Getter(value = AccessLevel.PROTECTED)
	protected final IFunction1<Object, IDescription> function;

	public ReassertObjectDescriber(IContext context) {
		final FunctionBuilder<Object, IDescription> builder = new TypeSwitch1.FunctionBuilder<>();
		builder.add(Object.class, v -> new IDescription() {
			@Override
			public String getIdentifier() {
				return v.toString();
			}

			@Override
			public String getName() {
				return v.toString();
			}
		});
		for (IDescriber<?> describer : context.getDescribers()) {
			add(builder, describer);
		}
		function = builder.build();
	}

	protected <T> void add(final FunctionBuilder<Object, IDescription> builder, IDescriber<T> describer) {
		final Class<T> type = describer.getType().getErasedType();
		builder.add(type, describer::describe);
	}

	@Override
	public IDescription apply(Object object) {
		return function.apply(object);
	}
}
