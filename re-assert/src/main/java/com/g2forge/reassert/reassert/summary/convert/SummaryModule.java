package com.g2forge.reassert.reassert.summary.convert;

import org.jgrapht.GraphPath;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import com.g2forge.reassert.core.algorithm.ReassertVertexDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.contract.IContract;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.reassert.convert.ReportRenderer;
import com.g2forge.reassert.term.eee.explain.convert.ExplanationMode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class SummaryModule extends SimpleModule {
	private static final long serialVersionUID = 840399436131646940L;

	protected final IContext context;

	protected ReportRenderer createReportRenderer() {
		return new ReportRenderer(ExplanationMode.Summarize);
	}

	protected ReassertVertexDescriber createVertexDescriber() {
		return new ReassertVertexDescriber(getContext());
	}

	@Override
	public void setupModule(SetupContext context) {
		super.setupModule(context);

		final ReassertVertexDescriber vertexDescriber = createVertexDescriber();
		context.addBeanSerializerModifier(new BeanSerializerModifier() {
			@Override
			public JsonSerializer<?> modifySerializer(SerializationConfig config, BeanDescription description, JsonSerializer<?> serializer) {
				if (ICoordinates.class.isAssignableFrom(description.getBeanClass())) return new CoordinateNameSerializer(vertexDescriber);
				if (IContract.class.isAssignableFrom(description.getBeanClass())) return new ContractSerializer(vertexDescriber);
				if (IFinding.class.isAssignableFrom(description.getBeanClass())) return new FindingSerializer(createReportRenderer());
				if (GraphPath.class.isAssignableFrom(description.getBeanClass())) return new PathSerializer(vertexDescriber);
				return serializer;
			}
		});
	}
}
