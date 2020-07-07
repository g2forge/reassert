package com.g2forge.reassert.list.convert;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.AnnotationIntrospector;
import com.fasterxml.jackson.databind.introspect.Annotated;
import com.fasterxml.jackson.databind.jsontype.NamedType;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.g2forge.alexandria.java.text.HString;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.model.work.IWorkType;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class GraphModule extends SimpleModule {
	@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "system")
	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	protected static abstract class PolymorphicCoordinatesMixin {
		@JsonIgnore
		public abstract ISystem<?> getSystem();
	}

	@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	protected static abstract class PolymorphicEdgeMixin {
		@JsonIgnore
		public abstract boolean isDirected();
	}

	@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	protected static abstract class PolymorphicVertexMixin {
		@JsonIgnore
		public abstract boolean isMaterial();
	}

	@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	protected static class PolymorphicWorkTypeMixin {}

	private static final long serialVersionUID = 3849430547992646737L;

	protected final IContext context;

	@Override
	public void setupModule(SetupContext context) {
		setMixInAnnotation(ICoordinates.class, PolymorphicCoordinatesMixin.class);
		setMixInAnnotation(IVertex.class, PolymorphicVertexMixin.class);
		setMixInAnnotation(IEdge.class, PolymorphicEdgeMixin.class);
		setMixInAnnotation(IWorkType.class, PolymorphicWorkTypeMixin.class);

		context.insertAnnotationIntrospector(new AnnotationIntrospector() {
			private static final long serialVersionUID = -2826606387145329925L;

			@Override
			public List<NamedType> findSubtypes(Annotated annotated) {
				if (ICoordinates.class.equals(annotated.getRawType())) {
					final List<NamedType> retVal = new ArrayList<>();
					for (ISystem<?> system : getContext().getSystems()) {
						final Class<?> type = system.getCoordinateType().getErasedType();
						final String name = HString.stripSuffix(type.getSimpleName(), "Coordinates").toLowerCase();
						retVal.add(new NamedType(type, name));
					}
					return retVal;
				}
				return null;
			}

			@Override
			public Version version() {
				return Version.unknownVersion();
			}
		});

		super.setupModule(context);
	}
}