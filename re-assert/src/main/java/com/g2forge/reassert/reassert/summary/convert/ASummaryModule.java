package com.g2forge.reassert.reassert.summary.convert;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.analyze.convert.ReportRenderer;
import com.g2forge.reassert.contract.eee.explain.convert.ExplanationMode;
import com.g2forge.reassert.core.algorithm.ReassertVertexDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.contract.IContract;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor
public abstract class ASummaryModule extends SimpleModule {
	private static final long serialVersionUID = 840399436131646940L;

	protected final IContext context;

	protected final IFunction1<? super ExplanationMode, ? extends ReportRenderer> rendererFactory;

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final ReassertVertexDescriber vertexDescriber = createVertexDescriber();

	protected ReassertVertexDescriber createVertexDescriber() {
		return new ReassertVertexDescriber(getContext());
	}

	protected JsonSerializer<?> modify(BeanDescription description, JsonSerializer<?> serializer) {
		if (ICoordinates.class.isAssignableFrom(description.getBeanClass())) return new CoordinateNameSerializer(getVertexDescriber());
		if (IContract.class.isAssignableFrom(description.getBeanClass())) return new ContractSerializer(getVertexDescriber());
		return serializer;
	}

	@Override
	public void setupModule(SetupContext context) {
		super.setupModule(context);
		context.addBeanSerializerModifier(new BeanSerializerModifier() {
			@Override
			public JsonSerializer<?> modifySerializer(SerializationConfig config, BeanDescription description, JsonSerializer<?> serializer) {
				return modify(description, serializer);
			}
		});
	}
}
