package com.g2forge.reassert.reassert.convert.finding;

import org.slf4j.event.Level;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.deser.BeanDeserializerModifier;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.model.TermConstant;
import com.g2forge.reassert.contract.model.findings.ExpressionContextualFinding;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.core.model.report.IContextualFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.expression.explain.model.IExplained;
import com.g2forge.reassert.expression.express.IExpression;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class FindingModule extends SimpleModule {
	protected static abstract class ContextualFindingMixin extends FindingMixin {
		@JsonIgnore
		public abstract Level getLevel();
	}

	@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
	@JsonInclude(JsonInclude.Include.NON_EMPTY)
	protected static abstract class ExplainedMixin {}

	@JsonInclude(JsonInclude.Include.NON_EMPTY)
	protected static abstract class ExpressionContextualFindingMixin extends ContextualFindingMixin {
		@JsonIgnore
		protected IExpression<TermRelation> expression;
	}

	@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	protected static abstract class FindingMixin {
		@JsonIgnore
		public abstract IFinding getInnermostFinding();
	}

	private static final long serialVersionUID = -1153968277342886689L;

	protected final IFunction1<IVertex, IDescription> vertexDescriber;

	@Override
	public void setupModule(SetupContext context) {
		this.setMixInAnnotation(IFinding.class, FindingMixin.class);
		this.setMixInAnnotation(IContextualFinding.class, ContextualFindingMixin.class);
		this.setMixInAnnotation(ExpressionContextualFinding.class, ExpressionContextualFindingMixin.class);
		this.setMixInAnnotation(IExplained.class, ExplainedMixin.class);
		super.setupModule(context);

		context.addBeanDeserializerModifier(new BeanDeserializerModifier() {
			public JsonDeserializer<?> modifyDeserializer(DeserializationConfig config, BeanDescription description, JsonDeserializer<?> deserializer) {
				if (TermConstant.class.isAssignableFrom(description.getBeanClass())) return new TermConstantDeserializer();
				return deserializer;
			}
		});
		context.addBeanSerializerModifier(new BeanSerializerModifier() {
			@Override
			public JsonSerializer<?> modifySerializer(SerializationConfig config, BeanDescription description, JsonSerializer<?> serializer) {
				if (TermConstant.class.isAssignableFrom(description.getBeanClass())) return new TermConstantSerializer(getVertexDescriber());
				return serializer;
			}
		});
	}
}