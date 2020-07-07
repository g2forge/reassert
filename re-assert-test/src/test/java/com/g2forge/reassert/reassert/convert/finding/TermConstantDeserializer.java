package com.g2forge.reassert.reassert.convert.finding;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.g2forge.alexandria.java.core.error.NotYetImplementedError;
import com.g2forge.reassert.core.model.contract.IContract;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.term.analyze.model.TermConstant;

public class TermConstantDeserializer extends StdDeserializer<TermConstant> {
	private static final long serialVersionUID = -7470240305968902587L;

	protected TermConstantDeserializer() {
		super(TermConstant.class);
	}

	@Override
	public TermConstant deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		final StoredTermConstant storedTermConstant = parser.readValuesAs(new TypeReference<StoredTermConstant>() {}).next();
		// Make up a contract, since we'd otherwise have to figure out how to load it from the graph, and we don't need that functionality yet
		return new TermConstant(storedTermConstant.getTerm(), new IContract() {
			@Override
			public String getName() {
				throw new NotYetImplementedError();
			}

			@Override
			public ITerms<?> getTerms() {
				throw new NotYetImplementedError();
			}
		});
	}
}